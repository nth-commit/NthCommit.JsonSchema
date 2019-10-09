using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Xunit;

namespace NthCommit.JsonSchema.CSharpTests
{
    public class NameTests
    {
        #region Reflect

        private static readonly Type[] Types = { typeof(JsonSchema) };

        private static IEnumerable<MemberInfo> Members => Types
            .SelectMany(t => t.GetMembers(
                BindingFlags.Public |
                BindingFlags.Instance |
                BindingFlags.Static |
                BindingFlags.DeclaredOnly));

        public static IEnumerable<MethodInfo> PublicMethods =>
            Members
                .OfType<MethodInfo>()
                .Where(mi => !mi.IsSpecialName && !mi.IsConstructor && !mi.Name.StartsWith("get_"));

        public static IEnumerable<PropertyInfo> PublicProperties =>
            Members
                .OfType<PropertyInfo>()
                .Where(pi => !pi.IsSpecialName);

        #endregion

        public static IEnumerable<object[]> PublicInterface =>
            Enumerable.Empty<MemberInfo>()
                .Concat(PublicMethods)
                .Concat(PublicProperties)
                .Select(mi => new object[] { mi });

        [Theory]
        [MemberData(nameof(PublicInterface))]
        public void PublicInterfaceFollowsDotNetNamingGuidelines(MemberInfo mi)
        {
            var startsWithUppercaseChar = char.IsUpper(mi.Name.First());
            Assert.True(startsWithUppercaseChar, $"{mi.Name} should start with uppercase letter");
        }

        public static IEnumerable<object[]> PublicMethodInterface =>
            PublicMethods.Select(mi => new object[] { mi });

        [Theory]
        [MemberData(nameof(PublicMethodInterface))]
        public void PublicMethodsDoNotReturnFSharpTypes(MethodInfo mi)
        {
            Assert.False(IsFSharpType(mi.ReturnType));
        }

        [Theory]
        [MemberData(nameof(PublicMethodInterface))]
        public void PublicMethodsDoNotHaveFSharpTypesAsParameters(MethodInfo mi)
        {
            Assert.Empty(mi.GetParameters().Where(pi => IsFSharpType(pi.ParameterType)));
        }

        private bool IsFSharpType(Type t) => t.Namespace.StartsWith("Microsoft.FSharp");
    }
}
