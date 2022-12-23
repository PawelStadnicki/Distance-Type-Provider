This type provider has been created according to: https://fsprojects.github.io/FSharp.TypeProviders.SDK/
but without the tests project.

This is a simple F# type provider.  It has separate design-time and runtime assemblies.

Paket is used to acquire the type provider SDK and build the nuget package (you can remove this use of paket if you like)

Building:

    dotnet tool restore
    
    
    dotnet build -c release

    dotnet paket pack nuget --version 0.0.2
