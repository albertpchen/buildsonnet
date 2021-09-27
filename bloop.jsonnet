{
    version: "1.4.0",
    project: {
        local project = self,
        ivyDep:: function (org, name, version) {
          org: org,
          name: name,
          version: version,
        },
        name: "parser",
        directory: "modules/parser",
        workspaceDir: "/home/achen2012/projects/scala3/build",
        sources: [
            project.directory + "/src/main/scala",
        ],
        dependencies: [],
        classpath: std.scala.cs([
          project.ivyDep("org.typelevel", "shapeless3-deriving_3", "3.0.3"),
          project.ivyDep("org.typelevel", "cats-parse_3", "0.3.4",),
          project.ivyDep("io.get-coursier", "coursier_2.13", "2.0.16"),
        ]),
        out: ".bloop/my-parser",
        classesDir: project.out + "/scala-3/classes",
        resources: [
            project.directory + "/src/main/resources",
        ],
        scala: {
            organization: "org.scala-lang",
            name: "scala-compiler",
            version: "3.0.2",
            options: [ ],
            jars: std.scala.cs([project.ivyDep("org.scala-lang", "scala3-compiler_3", "3.0.2")]),
            analysis: project.out + "/inc_compile_3.zip",
            setup: {
                "order": "mixed",
                "addLibraryToBootClasspath": true,
                "addCompilerToClasspath": false,
                "addExtraJarsToClasspath": false,
                "manageBootClasspath": true,
                "filterLibraryFromClasspath": true
            }
        },
        java: {
            "options": [
                
            ]
        },
        test: {
            frameworks: [
                {
                    "names": [
                        "munit.Framework"
                    ]
                }
            ],
            options: {
                excludes: [ ],
                arguments: [ ]
            }
        },
        platform: {
            name: "jvm",
            runtimeConfig: {
              options: [
                "-agentpath:/home/achen2012/tools/async-profiler-2.0-linux-x64/build/libasyncProfiler.so=start,event=cpu,file=profile.html"
              ]
            },
            config: {
                home: "/nix/store/v2167rhh9p6rqf329r69c8zcl1fzg0gd-openjdk-8u242-b08/lib/openjdk/jre",
                options: [
                    "-Duser.dir=" + project.workspaceDir
                ]
            },
            mainClass: [ ]
        },
        resolution: {
            modules: [
                // {
                //     organization: "org.scala-lang",
                //     name: "scala3-library_3",
                //     version: "3.0.2",
                //     configurations: "default",
                //     artifacts: [
                //         {
                //             name: "scala3-library_3",
                //             path: std.scala.cs([project.ivyDep("org.scala-lang", "scala-library", "2.13.6")]),
                //         }
                //     ]
                // },
            ]
        },
        tags: ["library"]
    }
}
