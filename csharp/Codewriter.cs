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

    public void pushtab() {
        tabs += tab;
    }
    public void poptab() {
        tabs = tabs.Substring(0, tabs.Length - 4);
    }

    public void startblock(string pre) {
        builder.AppendLine(tabs + pre + " {");
        pushtab();
    }

    public void endblock(string post = "") {
        poptab();
        builder.AppendLine(tabs + "} " + post);
    }

    public void include(string path) {
        head.AppendLine("#include \"" + path + "\"");
    }

    public void writeline(string line) {
        builder.AppendLine(tabs + line);
    }

    public void write(string str) => builder.Append(str);

    public override string ToString() {
        foreach (var file in fileDependencies) {
            
            var path = Path.GetRelativePath(plangFile.dirPath, file.hFilepath);
            include(path);
        }

        return head.ToString() + "\n" + builder.ToString();
    }
}