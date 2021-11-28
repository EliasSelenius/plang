using System.Text;
using System.Collections.Generic;
using System.IO;

class Codewriter {
    const string tab = "    ";

    PlangFile plangFile;

    StringBuilder builder = new();
    public StringBuilder head = new();
    string tabs = "";

    public List<PlangFile> fileDependencies = new();
    public void addDependencie(PlangFile file) {
        if (!fileDependencies.Contains(file)) fileDependencies.Add(file);
    }

    public Codewriter(PlangFile pFile) {
        plangFile = pFile;
    }

    public void startblock(string pre) {
        builder.AppendLine(tabs + pre + " {");
        tabs += tab;
    }

    public void endblock(string post = "") {
        tabs = tabs.Substring(0, tabs.Length - 4);
        builder.AppendLine(tabs + "} " + post);
    }

    public void include(string path) {
        head.AppendLine("#include \"" + path + "\"");
    }

    public void writeline(string line) {
        builder.AppendLine(tabs + line);
    }

    public override string ToString() {
        foreach (var file in fileDependencies) {
            
            var path = Path.GetRelativePath(plangFile.dirPath, file.hFilepath);
            include(path);
        }

        return head.ToString() + "\n" + builder.ToString();
    }
}