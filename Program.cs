using System;
using System.Collections.Generic;
using System.IO;

using pcombinator;
using static pcombinator.Parser;

/*

    Big selling points:
        - declaration order independent
        - no headers or includes
        - type inference
        - modules
        - overloads
        - generics
        - operator overload


    TODOs:
        *- boolean operators and comparisons 
        - function pointers
        - function overloads
        - member functions
        - forgot semicolon error message?
        - return type inference
        - subfunctions
        *- while, for i in 0..10, loop, break, continue
        *- else if

    InProgress:
        *- pointer operators ( . -> * & )
        - keep track of stack variables (for type inference and syntax checking)

    Done:
        - <Done> Typename distinguish struct and primitive type
        *- <Done> resolve header includes
        - <Done> better number literal type inference
        - <Done> proper struct field transpilation 
        *- <Kinda Done> proper expression parsing
        - <Done> function arguments
        - <Kinda Done> single line comments
        - <Kinda Done> multi line comments
*/

static class Global {

    public static PlangFile currentFile;

    public static Dictionary<string, PlangFile> fileDependencies = new();

    public static List<Function> functions = new();

    public static void addFunction(Function func) {
        functions.Add(func);
        fileDependencies[func.name] = currentFile;
    }

    public static void validate() {
        foreach (var func in functions) {
            func.codeblock.validate();
        }
    }

    public static Dictionary<string, StructDeclaration> structs = new();
    public static bool getStruct(string name, out StructDeclaration str) => structs.TryGetValue(name, out str);
}


class PlangFile {
    // filepath relative to project top directory
    public readonly string filepath;
    public readonly string dirPath;
    public readonly string cFilepath, hFilepath;

    public State parsedState; 


    public PlangFile(string filepath) {
        this.filepath = filepath;

        cFilepath = Path.ChangeExtension(filepath, ".g.c");
        hFilepath = Path.ChangeExtension(filepath, ".g.h");

        dirPath = Path.GetDirectoryName(filepath);
        if (dirPath == string.Empty) dirPath = ".";
        
    }

}

class PlangProject {
    public string projDir, outputDir;
    public List<PlangFile> files = new();

    public List<string> errors = new();

    public PlangProject(string proj) {
        projDir = proj;
        outputDir = Path.Combine(projDir, "output");

    }

    public string getClangCmd() {
        var cmd = "";
        foreach (var file in files) {
            cmd += " " + Path.Combine(outputDir, file.cFilepath);
        }
        cmd += " -o " + Path.Combine(projDir, "Program.exe");

        return cmd.Replace('\\', '/');
    }

    public void parse() {
        foreach (var filename in Directory.EnumerateFiles(projDir, "*.txt", SearchOption.AllDirectories)) {
            Global.currentFile = new(Path.GetRelativePath(projDir, filename));
            files.Add(Global.currentFile);

            var state = Plang.mainParser.run(File.ReadAllText(filename));
            if (state.isError) Program.error(state.errorMsg);
            
            Global.currentFile.parsedState = state;
        }

        foreach (var item in files) System.Console.WriteLine(item.filepath + " " + item.parsedState.isError);
    }

    public void transpile() {

        Directory.CreateDirectory(outputDir);

        // transpile
        foreach (var file in files) {
            
            // ensure subdirectory exists
            var dirName = Path.GetDirectoryName(file.filepath);
            if (!string.IsNullOrWhiteSpace(dirName)) Directory.CreateDirectory(Path.Combine(outputDir, dirName));
            

            (Program.cFile, Program.hFile) = (new(file), new(file));

            // pragam once and header include
            Program.hFile.head.AppendLine("#pragma once");
            //Program.cFile.include(Path.GetFileName(file.hFilepath));
            Program.cFile.addDependencie(file);

            // transpile
            foreach (var item in file.parsedState.result) {
                item.transpile();
            }
            
            // write transpiled result to files
            File.WriteAllText(Path.Combine(outputDir, file.cFilepath), Program.cFile.ToString());
            File.WriteAllText(Path.Combine(outputDir, file.hFilepath), Program.hFile.ToString());
        }
    }
}


static class Program {

    public static Codewriter cFile, hFile;


    static void Main(string[] args) {

        //runParserTests();
        System.Console.WriteLine("directory: " + Directory.GetCurrentDirectory());
        
        //Plang.newExpressionsTest();


        if (args.Length == 1) {
            
            var project = new PlangProject(args[0]);
            project.parse();

            Global.validate();

            project.transpile();

            System.Diagnostics.Process.Start("clang", project.getClangCmd()).WaitForExit();
            System.Diagnostics.Process.Start(Path.Combine(project.projDir, "Program.exe"));

        } else {
            System.Console.WriteLine("wrong argument format.");
        }
    }


    public static void error(string message) {
        System.Console.WriteLine(message);
    }

    public static void warn(string message) {

    }
}
