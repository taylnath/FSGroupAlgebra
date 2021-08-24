FROM mcr.microsoft.com/dotnet/runtime:5.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /src
COPY ["FSGroupAlgebra/FSGroupAlgebra.fsproj", "FSGroupAlgebra/"]
RUN dotnet restore "FSGroupAlgebra/FSGroupAlgebra.fsproj"
COPY . .
WORKDIR "/src/FSGroupAlgebra"
RUN dotnet build "FSGroupAlgebra.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "FSGroupAlgebra.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "FSGroupAlgebra.dll"]
