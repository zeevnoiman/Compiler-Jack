import java.io.{File, PrintWriter}
import javax.swing.JFileChooser
import scala.io.Source

class Labeler (){
  private var _currentValue = 0
  def getLabel: String = {
    _currentValue += 1
    _currentValue.toString
  }
}

object main {

  val myLabeler = new Labeler

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def getConstLabel (orderType: String) : String = {
    orderType match {
      case "local" => "LCL"
      case "argument" => "ARG"
      case "this" => "THIS"
      case "that" => "THAT"
      case "static" => "16"
      case "pointer" => "3"
      case "temp" => "5"
    }
  }

  def selectVMDir(): String = {
    val chooser = new JFileChooser
    chooser.setCurrentDirectory(new File("C:\\Users\\אלישיב\\Desktop\\מדעי המחשב\\שנה ג סמסטר ב\\קורס עקרונות שפת תוכנה\\תרגיל 1\\nand2tetris\\projects"))
    chooser.setDialogTitle("Choose dir for compile from vm to asm")
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
    chooser.setAcceptAllFileFilterUsed(true)
    if (chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
      val selectedFile = chooser.getSelectedFile
      if (selectedFile.isDirectory)
        return selectedFile.toString
      else
        return selectedFile.toString.substring(0, selectedFile.toString.lastIndexOf("\\"))
    }
    "NULL"
  }

  def AddToAsm(asmFile: PrintWriter) {
    asmFile.write("@SP\nM=M-1\n@SP\nA=M\nD=M\n@SP\nM=M-1\n@SP\n" +
      "A=M\nA=M\nD=D+A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n")
  }

  def SubToAsm(asmFile: PrintWriter) {
    asmFile.write("@SP\nM=M-1\n@SP\nA=M\nD=M\n@SP\nM=M-1\n" +
      "@SP\nA=M\nA=M\nD=A-D\n@SP\nA=M\nM=D\n@SP\nM=M+1\n")
  }

  def NegToAsm(asmFile: PrintWriter) {
    asmFile.write("@SP\nA=M-1\nD=M\nM=-D\n")
  }

  def EqToAsm(asmFile: PrintWriter) {
    val label_1 = "CompLabel" + myLabeler.getLabel
    val label_2 = "CompLabel" + myLabeler.getLabel
    asmFile.write("@SP\nM=M-1\n@SP\nA=M\nD=M\n@SP\nM=M-1\n@SP\nA=M\nA=M\nD=D-A\n@" +
      label_1 + "\nD ; JEQ\n@" + label_2 + "\nD=0; JEQ\n(" + label_1 + ")\nD=-1\n(" +
      label_2 + ")\n@SP\nA=M\nM=D\n@SP\nM=M+1\n")
  }

  def GtToAsm(asmFile: PrintWriter) {
    val label_1 = "CompLabel" + myLabeler.getLabel
    val label_2 = "CompLabel" + myLabeler.getLabel
    asmFile.write("@SP\nM=M-1\n@SP\nA=M\nD=M\n@SP\nM=M-1\n@SP\nA=M\nA=M\nD=A-D\n@" +
      label_1 + "\nD ; JGT\n@" + label_2 + "\nD=0 ; JEQ\n(" + label_1 + ")\nD=-1\n(" +
      label_2 + ")\n@SP\nA=M\nM=D\n@SP\nM=M+1\n")
  }

  def LtToAsm(asmFile: PrintWriter) {
    val label_1 = "CompLabel" + myLabeler.getLabel
    val label_2 = "CompLabel" + myLabeler.getLabel
    asmFile.write("@SP\nM=M-1\nA=M\nD=M\nA=A-1\nA=M\nD=A-D\n@"
      +label_1 + "\nD ; JLT\n@SP\nA=M-1\nM=0\n@" + label_2 + "\n0 ; JEQ\n("
      + label_1 + ")\n@SP\nA=M-1\nM=-1\n(" + label_2 + ")\n")
  }

  def AndToAsm(asmFile: PrintWriter) {
    asmFile.write("@SP\nA=M\nA=A-1\nD=M\nA=A-1\nM=M&D\n@SP\nM=M-1\n")
  }

  def OrToAsm(asmFile: PrintWriter) {
    asmFile.write("@SP\nA=M\nA=A-1\nD=M\nA=A-1\nM=M|D\n@SP\nM=M-1\n")
  }

  def NotToAsm(asmFile: PrintWriter) {
    asmFile.write("@SP\nA=M-1\nM=!M\n")
  }

  def PushToAsm(asmFile: PrintWriter, orderType: String, value :String) {
    orderType match {
      case "constant" => asmFile.write("@" + value + "\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n")
      case "local" | "argument" | "this" | "that" => asmFile.write("@" + value + "\nD=A\n@" +
        getConstLabel(orderType) + "\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n")
      case "pointer" | "temp" => asmFile.write("@" + value + "\nD=A\n@" + getConstLabel(orderType) +
        "\nA=A+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n")
      case _ => println("ERROR - Got unexpected token: ~" + orderType + "~ .")
    }
  }

  def PushStaticToAsm(asmFile: PrintWriter, value: String, pathOfFile: String) {
    val fileName = pathOfFile.split('\\').last
    asmFile.write("@" + fileName + "." + value + "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n")
  }

  def PopToAsm(asmFile: PrintWriter, orderType: String, value: String) {
    orderType match {
      case "constant" => asmFile.write("@SP\nM=M-1\n")
      case "local" | "argument" | "this" | "that" => asmFile.write("@SP\nM=M-1\n@" + value +
        "\nD=A\n@" + getConstLabel(orderType) + "\nD=M+D\n@R13\nM=D\n@SP\nA=M\nD=M\n@R13\nA=M\nM=D\n")
      case "pointer" | "temp" => asmFile.write("@SP\nM=M-1\n@" + value + "\nD=A\n@" + getConstLabel(orderType) +
        "\nD=A+D\n@R13\nM=D\n@SP\nA=M\nD=M\n@R13\nA=M\nM=D\n")
      case _ => println("ERROR - Got unexpected token: ~" + orderType + "~ .")
    }
  }

  def PopStaticToAsm (asmFile: PrintWriter, value: String, pathOfFile: String) {
    val fileName = pathOfFile.split('\\').last
    asmFile.write("@SP\nM=M-1\n@SP\nA=M\nD=M\n@" + fileName + "." + value + "\nM=D\n")
  }

  def CallToAsm (asmFile: PrintWriter, functionName: String, numOfArgs: String) {
    val Label = "FuncLabel" + myLabeler.getLabel
    asmFile.write("@" + Label + "\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@LCL\nD=M\n@SP\nA=M\nM=D\n" +
      "@SP\nM=M+1\n@ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THAT\n" +
      "D=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@" + numOfArgs + "\nD=A\n@5\nD=D+A\n@SP\nD=M-D\n@ARG\nM=D\n@SP\n" +
      "D=M\n@LCL\nM=D\n@" + functionName + "\n1;JMP\n(" + Label + ")\n")
  }

  def FunctionToAsm (asmFile: PrintWriter, functionName: String, numOfLocals: String) {
    asmFile.write("(" + functionName + ")\n")
    for (_ <- 1 to numOfLocals.toInt) {
      PushToAsm(asmFile, "constant", "0")
    }
  }

  def ReturnToAsm (asmFile: PrintWriter) {
    asmFile.write("@LCL\nD=M\n@5\nA=D-A\nD=M\n@R14\nM=D\n")
    PopToAsm(asmFile, "argument", "0")
    asmFile.write("@ARG\nD=M+1\n@SP\nM=D\n@LCL\nD=M\n@1\nA=D-A\nD=M\n@THAT\nM=D\n@LCL\nD=M\n" +
      "@2\nA=D-A\nD=M\n@THIS\nM=D\n@LCL\nD=M\n@3\nA=D-A\nD=M\n@ARG\nM=D\n@LCL\nD=M\n@4\n" +
      "A=D-A\nD=M\n@LCL\nM=D\n@R14\nA=M\n1;JMP\n")
  }

  def LabelToAsm (asmFile: PrintWriter, value: String, pathOfFile: String) {
    val fileName = pathOfFile.split('\\').last
    asmFile.write("(" + fileName + "$" + value + ")\n")
  }

  def GotoToAsm(asmFile: PrintWriter, value: String, pathOfFile: String) {
    val fileName = pathOfFile.split('\\').last
    asmFile.write("@" + fileName + "$" + value + "\n1;JMP\n")
  }

  def IfGoToToAsm(asmFile: PrintWriter, value: String, pathOfFile: String) {
    val fileName = pathOfFile.split('\\').last
    asmFile.write("@SP\nM=M-1\nA=M\nD=M\n@" + fileName + "$" + value + "\nD;JNE\n")
  }

  def translateFile(pathOfFile: File, asmFile: PrintWriter) {
    val vmFileLines = Source.fromFile(pathOfFile.getPath).getLines()
    for (line <- vmFileLines){
      if (line.startsWith("add")){
        AddToAsm(asmFile)
      } else if (line.startsWith("sub")){
        SubToAsm(asmFile)
      } else if (line.startsWith("neg")){
        NegToAsm(asmFile)
      } else if (line.startsWith("eq")){
        EqToAsm(asmFile)
      } else if (line.startsWith("gt")) {
        GtToAsm(asmFile)
      } else if (line.startsWith("lt")) {
        LtToAsm(asmFile)
      } else if (line.startsWith("and")) {
        AndToAsm(asmFile)
      } else if (line.startsWith("or")) {
        OrToAsm(asmFile)
      } else if (line.startsWith("not")) {
        NotToAsm(asmFile)
      } else if (line.startsWith("push static")) {
        PushStaticToAsm(asmFile, line.split(' ')(2), pathOfFile.toString.replace(".vm", ""))
      } else if (line.startsWith("push")) {
        PushToAsm(asmFile, line.split(' ')(1), line.split(' ')(2))
      } else if (line.startsWith("pop static")) {
        PopStaticToAsm(asmFile, line.split(' ')(2), pathOfFile.toString.replace(".vm", ""))
      } else if (line.startsWith("pop")){
        PopToAsm(asmFile, line.split(' ')(1), line.split(' ')(2))
      } else if (line.startsWith("call")){
        CallToAsm(asmFile, line.split(' ')(1), line.split(' ')(2))
      } else if (line.startsWith("return")){
        ReturnToAsm(asmFile)
      } else if (line.startsWith("function")){
        FunctionToAsm(asmFile, line.split(' ')(1), line.split(' ')(2))
      }else if(line.startsWith("label")){
        LabelToAsm(asmFile, line.split(' ')(1),pathOfFile.toString.replace(".vm", ""))
      }else if(line.startsWith("goto")){
        GotoToAsm(asmFile, line.split(' ')(1), pathOfFile.toString.replace(".vm", ""))
      }else if(line.startsWith("if-goto")){
        IfGoToToAsm(asmFile, line.split(' ')(1), pathOfFile.toString.replace(".vm", ""))
      } else if (line.startsWith("//")){
        //That is a comment..
      } else {
        println("ERROR - Got unexpected token: ~" + line + "~ .")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var pathOfDir  = selectVMDir()
    if (pathOfDir == "NULL") {
      println("Enter path of vm's files dir: ")
      pathOfDir = scala.io.StdIn.readLine()
    }
    println(pathOfDir)
    val asmFileName = pathOfDir.split('\\').last
    val asmFile : PrintWriter = new PrintWriter(
      new File(pathOfDir + "\\" + asmFileName + ".asm"))
    if (asmFileName.startsWith("FibonacciElement") ||
      asmFileName.startsWith("NestedCall") || asmFileName.startsWith("StaticsTest")) {
      asmFile.write("@256\nD=A\n@SP\nM=D\n")
      CallToAsm(asmFile, "Sys.init", "0")
      asmFile.write("@END\n0;JMP\n")
    }
    val listOfFiles = getListOfFiles(pathOfDir)
    for (pathOfFile <- listOfFiles) {
      if (pathOfFile.getPath.endsWith(".vm")) {
        translateFile(pathOfFile, asmFile)
      }
    }
    asmFile.close()
  }
}