import java.io.{File, PrintWriter}
import javax.swing.JFileChooser
import TokenType.TokenType
import scala.io.Source
import scala.collection.mutable.ListBuffer

//Enum for represent the token type
object TokenType extends Enumeration {
  type TokenType = Value
  val keyword, identifier, integerConstant, symbol,
  stringConstant, eof, error, start = Value
}

object SymbolType extends Enumeration {
  type SymbolType = Value
  val STATIC, FIELD, ARG, VAR, NONE = Value
}

class SymbolTable {

  var classList: ListBuffer[(String, String, SymbolType.SymbolType, Int)] = ListBuffer()
  var subroutineList: ListBuffer[(String, String, SymbolType.SymbolType, Int)] = ListBuffer()
  var indexStatic : Int = 0
  var indexField : Int = 0
  var indexArgument : Int = 0
  var indexVar : Int = 0

  def startSubroutine(): Unit = {
    subroutineList.clear()
    indexArgument = 0
    indexVar = 0
  }

  def define(name: String, s_type: String, kind: SymbolType.SymbolType): Unit = {
    if (kind == SymbolType.ARG) {
      subroutineList.append((name, s_type, kind, indexArgument))
      indexArgument += 1
    } else if (kind == SymbolType.VAR){
      subroutineList.append((name, s_type, kind, indexVar))
      indexVar += 1
    } else if (kind == SymbolType.FIELD){
      classList.append((name, s_type, kind, indexField))
      indexField += 1
    } else if (kind == SymbolType.STATIC){
      classList.append((name, s_type, kind, indexStatic))
      indexStatic += 1
    } else{
      print("ERROR")
    }
  }

  def varCount(kind: SymbolType.SymbolType): Int = {
    var sum: Int = 0
    for (element <- classList){
      if (element._3 == kind){
        sum += 1
      }
    }
    for (element <- subroutineList){
      if (element._3 == kind){
        sum += 1
      }
    }
    sum
  }

  def kindOf (name: String): String = {
    def convertKindToVM(symbol: SymbolType.SymbolType) : String = {
      symbol match {
        case SymbolType.FIELD => "this"
        case SymbolType.ARG => "argument"
        case SymbolType.STATIC => "static"
        case SymbolType.VAR => "local"
        case SymbolType.NONE => "ERROR"
      }

    }

    var result: String = ""
    for ((e_name, _, e_kind, _) <- classList){
      if (e_name == name){
        result = convertKindToVM(e_kind)
      }
    }
    for ((e_name, _, e_kind, _) <- subroutineList){
      if (e_name == name){
        result = convertKindToVM(e_kind)
      }
    }
    if (result == ""){
      result = convertKindToVM(SymbolType.NONE)
    }
    result
  }

  def typeOf (name: String): String = {
    var result: String = ""
    for (element <- classList){
      if (element._1 == name){
        result = element._2.toString
      }
    }
    for (element <- subroutineList){
      if (element._1 == name){
        result = element._2.toString
      }
    }
    result
  }

  def indexOf (name: String): Int = {
    var result: Int = -1
    for (element <- classList){
      if (element._1 == name){
        result = element._4
      }
    }
    for (element <- subroutineList){
      if (element._1 == name){
        result = element._4
      }
    }
    result
  }
}


//Part 1: from .jack to T.xml
class JackTokenizer (pathOfJackFile : File){

  //List of keywords
  val keywords = List("class", "constructor", "class", "method", "function",
    "int", "boolean", "char", "void", "var", "static", "field", "let", "do",
    "if", "else", "while", "return", "true", "false", "null", "this")

  //List of symbols
  val symbols = List('{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-',
    '*', '/', '&', '|', '<', '>', '=', '~')

  //.jack file as string
  val jackFileContent : String = getContentOfFileWithPreProcessing(pathOfJackFile)

  //Pointer on the start of every token
  var mainIndex :Int = 0

  //Remove comments and '\n's from the .jack file
  def getContentOfFileWithPreProcessing (file: File): String = {
    var strWithoutComments: String = ""
    for (line <- Source.fromFile(pathOfJackFile.getPath).getLines){
      if (!line.startsWith("//"))
        strWithoutComments += line.split("//")(0)
    }
    strWithoutComments.replace('\t', ' ')
  }

  //Get the char that the pointer points on
  def getChar(index: Int):Char = jackFileContent(mainIndex + index)

  //Check if end of file
  def isEOF(index: Int):Boolean = mainIndex + index >= jackFileContent.length

  //Check if letter
  def isLetter(c: Char):Boolean = (c >='a' && c <= 'z' ) || (c >= 'A' && c <= 'Z')

  def isUnderLine(c: Char):Boolean = c == '_'

  def isDigit(c: Char):Boolean = c >= '0' && c <= '9'

  def isSymbol(c: Char):Boolean = symbols.contains(c)

  def isQuote(c: Char):Boolean = c == '\"'

  //Return the next token
  def getToken: (String, TokenType) = {

    //The function's vars
    var relativeIndex : Int = 0

    //The function's help-functions
    //State for keywords
    def q1(): (String, TokenType) = {
      var result: String = ""
      var tokenType: TokenType = TokenType.start
      while (!isEOF(relativeIndex) &&
        (isLetter(getChar(relativeIndex)) || isDigit(getChar(relativeIndex)) || isUnderLine(getChar(relativeIndex)))){
        relativeIndex += 1
      }
      result = jackFileContent.slice(mainIndex, mainIndex + relativeIndex)
      if (keywords.contains(result)) {
        tokenType = TokenType.keyword
      } else {
        tokenType = TokenType.identifier
      }
      mainIndex += relativeIndex
      (result, tokenType)
    }

    //State for identifiers
    def q2(): (String, TokenType) = {
      var result: String = ""
      while (!isEOF(relativeIndex) && isDigit(getChar(relativeIndex))){
        relativeIndex += 1
      }
      result = jackFileContent.slice(mainIndex, mainIndex + relativeIndex)
      mainIndex += relativeIndex
      (result, TokenType.integerConstant)
    }

    //State for integerConstant
    def q3(): (String, TokenType) = {
      relativeIndex += 1
      val result: String = jackFileContent.slice(mainIndex, mainIndex + relativeIndex)
      mainIndex += relativeIndex
      (result, TokenType.symbol)
    }

    //State for symbols
    def q4(): (String, TokenType) = {
      var result: String = ""
      relativeIndex += 1
      while (!isEOF(relativeIndex) && !isQuote(getChar(relativeIndex))){
        relativeIndex += 1
      }
      relativeIndex += 1
      result = jackFileContent.slice(mainIndex + 1, mainIndex + relativeIndex - 1)
      mainIndex += relativeIndex
      (result, TokenType.stringConstant)
    }

    //State for complicated comments (/*)
    def q5(): (String, TokenType) = {
      while (!(getChar(relativeIndex) == '*' && getChar(relativeIndex + 1) == '/')){
        mainIndex += 1
      }
      mainIndex += 2
      q0()
    }

    //Initial state
    def q0(): (String, TokenType) = {
      if (isEOF(relativeIndex)) {
        return ("EOF", TokenType.eof)
      }
      var char: Char = getChar(relativeIndex)
      while (char == ' ') {
        mainIndex += 1
        if (isEOF(relativeIndex)) {
          return ("EOF", TokenType.eof)
        }
        char = getChar(relativeIndex)
      }
      if (getChar(relativeIndex) == '/' && getChar(relativeIndex + 1) == '*') {
        q5()
      } else if (isLetter(char) || isUnderLine(char)) {
        q1()
      } else if (isDigit(char)) {
        q2()
      } else if (isSymbol(char)) {
        q3()
      } else if (isQuote(char)) {
        q4()
      } else if (isEOF(relativeIndex)) {
        ("EOF", TokenType.eof)
      } else {
        ("ERROR", TokenType.error)
      }
    }

    //The function's main
    q0()
  }
}

class Parsing(pathOfXMLTFile : File, vmFile : PrintWriter){

  var indentation : Int = 0
  var indexOfTokenList : Int = 0
  var tokenList : List[String] = _
  var symbolTable : SymbolTable = new SymbolTable()
  var className: String = _
  var conditionIndex : Int = 0
  var whileIndex : Int = 0

  //Subroutine level
  var numberOfSubRoutineLocalVars : Int = _
  var SubRoutineKind : String = _
  var SubRoutineName : String = _

  //call level
  var numberOfCallToParams : Int = _

  //initial call
  Parse()

  def Parse(): Unit = {
    tokenList = Source.fromFile(pathOfXMLTFile).getLines().toList
    goToken() // <>tokens<>
    if(getContentOfToken == "class") {
      ParseClass()
    }
  }

  def ParseClass(): Unit = {
    goToken() // <>class<>
    className = getContentOfToken
    goToken() // <>nameClass<>
    goToken() // <>{<>
    while (getContentOfToken == "static" || getContentOfToken == "field") {
      ParseClassVarDec()
    }
    while (getContentOfToken == "constructor" || getContentOfToken == "function"
      || getContentOfToken == "method") {
      ParseSubRoutineDec()
    }
    goToken() // <>}<>
  }

  def ParseClassVarDec() : Unit={
    val kind : SymbolType.SymbolType = SymbolType.withName(getContentOfToken.toUpperCase)
    goToken() // <>static<> || <>field<>
    val s_type : String = getContentOfToken
    goToken() // <>type<>
    var name : String = getContentOfToken
    symbolTable.define(name, s_type, kind)
    goToken() // <>name<>
    while(getContentOfToken == ","){
      goToken() // <>,<>
      name = getContentOfToken
      goToken() // <>name<>
      symbolTable.define(name, s_type, kind)
    }
    goToken() // <>;<>
  }

  def ParseSubRoutineDec(): Unit={
    SubRoutineKind = getContentOfToken
    goToken() // <>constructor<> || <>function<> || <>method<>
    symbolTable.startSubroutine()
    conditionIndex = 0
    if (SubRoutineKind == "method"){
      symbolTable.define("this", className, SymbolType.ARG)
    }
    goToken() // <>type<> || <>void<>
    SubRoutineName = getContentOfToken
    goToken() // <>sub routine name<>
    goToken() // <>(<>
    ParseParameterList()
    goToken() // <>)<>
    ParseSubRoutineBody()
  }

  def ParseParameterList(): Unit={
    val kind : SymbolType.SymbolType = SymbolType.ARG
    if(getContentOfToken != ")"){
      var s_type : String = getContentOfToken
      goToken() // <>type<>
      var name : String = getContentOfToken
      goToken() // <>varName<>
      symbolTable.define(name, s_type, kind)
      while(getContentOfToken == ","){
        goToken() // <>,<>
        s_type = getContentOfToken
        goToken() // <>type<>
        name = getContentOfToken
        goToken() // <>varName<>
        symbolTable.define(name, s_type, kind)
      }
    }
  }

  def ParseSubRoutineBody(): Unit={
    numberOfSubRoutineLocalVars = 0
    goToken() // <>{<>
    while(getContentOfToken == "var"){
      ParseVarDec()
    }
    SubRoutineKind match {
      case "constructor" => vmFile.write("function " + className + ".new " + numberOfSubRoutineLocalVars.toString +
        "\npush constant " + symbolTable.varCount(SymbolType.FIELD).toString +
        "\ncall Memory.alloc 1\npop pointer 0\n")
      case "method" => vmFile.write("function " + className + "." + SubRoutineName + " " +
        numberOfSubRoutineLocalVars.toString + "\npush argument 0\npop pointer 0\n")
      case "function" => vmFile.write("function " + className + "." + SubRoutineName + " " +
        numberOfSubRoutineLocalVars.toString + "\n")
    }
    ParseStatements()
    goToken() // <>}<>
  }

  def ParseVarDec(): Unit = {
    val kind : SymbolType.SymbolType = SymbolType.VAR
    goToken() // <>var<>
    val s_type : String = getContentOfToken
    goToken() // <>type<>
    var name : String = getContentOfToken
    goToken() // <>varName<>
    numberOfSubRoutineLocalVars += 1
    symbolTable.define(name, s_type, kind)
    while(tokenList(indexOfTokenList).contains(",")){
      goToken() // <>,<>
      numberOfSubRoutineLocalVars += 1
      name = getContentOfToken
      goToken() // <>varName<>
      symbolTable.define(name, s_type, kind)
    }
    goToken() // <>;<>
  }

  def ParseStatements() : Unit={
    while(tokenList(indexOfTokenList).contains("let") ||
      tokenList(indexOfTokenList).contains("if") ||
      tokenList(indexOfTokenList).contains("while") ||
      tokenList(indexOfTokenList).contains("do") ||
      tokenList(indexOfTokenList).contains("return")) {
      ParseStatement()
    }
  }

  def ParseStatement(): Unit={
    getContentOfToken match {
      case "let" => ParseLetStatement()
      case "if" => ParseIfStatement()
      case "while" => ParseWhileStatement()
      case "do" => ParseDoStatement()
      case "return" => ParseReturnStatement()
    }
  }

  def ParseLetStatement(): Unit={
    goToken() // <>let<>
    val name : String = getContentOfToken
    goToken() // <>varName<>
    val isAssignment : Boolean = getContentOfToken == "["
    if(isAssignment){
      goToken() // <>[<>
      ParseExpression()
      goToken() // <>]<>
      vmFile.write("push " + symbolTable.kindOf(name).toString + " " +
        symbolTable.indexOf(name).toString + "\nadd\n")
      goToken() // <>=<>
      ParseExpression()
      vmFile.write("pop temp 0\npop pointer 1\npush temp 0\npop that 0\n")
      goToken() // <>;<>
    }
    else {
      goToken() // <>=<>
      ParseExpression()
      vmFile.write("pop " + symbolTable.kindOf(name).toString + " " + symbolTable.indexOf(name).toString + "\n")
      goToken() // <>;<>
    }
  }

  def ParseIfStatement(): Unit={
    goToken() // <>if<>
    val indexOfIf : String = conditionIndex.toString
    conditionIndex += 1
    goToken() // <>(<>
    ParseExpression()
    goToken() // <>)<>
    goToken() // <>{<>
    vmFile.write("if-goto IF_TRUE" + indexOfIf + "\ngoto IF_FALSE" +
      indexOfIf + "\nlabel IF_TRUE" + indexOfIf + "\n")
    ParseStatements()
    goToken() // <>}<>
    if(getContentOfToken == "else"){
      vmFile.write("goto IF_END" + indexOfIf + "\nlabel IF_FALSE" + indexOfIf + "\n")
      goToken() // <>else<>
      goToken() /// <>{<>
      ParseStatements()
      goToken() // <>}<>
      vmFile.write("label IF_END" + indexOfIf + "\n")
    } else {
      vmFile.write("label IF_FALSE" + indexOfIf + "\n")
    }
  }

  def ParseWhileStatement(): Unit = {
    val whileStatementIndex : String = whileIndex.toString
    whileIndex += 1
    goToken() // <>while<>
    vmFile.write("label WHILE_EXP" + whileStatementIndex + "\n")
    goToken() // <>(<>
    ParseExpression()
    goToken() // <>)<>
    vmFile.write("not\nif-goto WHILE_END" + whileStatementIndex + "\n")
    goToken() // <>{<>
    ParseStatements()
    goToken() // <>}<>
    vmFile.write("goto WHILE_EXP" + whileStatementIndex + "\nlabel WHILE_END" + whileStatementIndex + "\n")
  }

  def ParseDoStatement(): Unit={
    goToken() // <>do<>
    ParseSubRoutineCall()
    vmFile.write("pop temp 0\n")
    goToken() //<>;<>
  }

  def ParseReturnStatement(): Unit={
    goToken() // <>return<>
    if(getContentOfToken != ";"){
      ParseExpression()
    } else {
      vmFile.write("push constant 0\n")
    }
    vmFile.write("return\n")
    goToken() // <>;<>
  }

  def ParseSubRoutineCall() : Unit = {
    val functionOrVarName: String = getContentOfToken
    goToken() // <>SubRoutineName<> || <>className<> || <>varName<>
    //if <>SubRoutineName<>
    if (getContentOfToken == "(") {
      vmFile.write("push pointer 0\n")
      numberOfCallToParams = 1
      goToken() // <>(<>
      ParseExpressionList()
      goToken() // <>)<>
      vmFile.write("call " + className + "." + functionOrVarName + " " +
        numberOfCallToParams.toString + "\n")
    }
    //if <>className<> || <>varName<>
    else if (getContentOfToken == ".") {
      goToken() // <>.<>
      val SubRoutineCallName: String = getContentOfToken
      goToken() // <>SubRoutineCallName<>
      goToken() // <>(<>
      //<>className<>
      if (symbolTable.indexOf(functionOrVarName) == -1) {
        numberOfCallToParams = 0
        ParseExpressionList()
        goToken() // <>)<>
        vmFile.write("call " + functionOrVarName + "." + SubRoutineCallName + " " +
          numberOfCallToParams + "\n")
      }
      // if <>varName<>
      else {
        numberOfCallToParams = 1
        vmFile.write("push " + symbolTable.kindOf(functionOrVarName) + " " +
          symbolTable.indexOf(functionOrVarName) + "\n")
        ParseExpressionList()
        goToken() // <>)<>
        vmFile.write("call " + symbolTable.typeOf(functionOrVarName) + "." + SubRoutineCallName + " " +
          numberOfCallToParams.toString + "\n")
        numberOfCallToParams -= 1
      }
    }
  }

  def ParseExpression(): Unit = {
    ParseTerm()
    val binaryOps = List("+", "-", "*", "/", "&", "|", "&lt;", "&gt;", "&amp;",  "=")
    while(binaryOps.contains(getContentOfToken)) {
      val operatorType: String = getContentOfToken
      goToken() // <>+<> || <>-<> || <>*<> || <>/<> || <>&<> || <>|<> || <&lt;<> || <>&gt<> || <>=<>
      ParseTerm()
      operatorType match {
        case "+" => vmFile.write("add\n")
        case "-" => vmFile.write("sub\n")
        case "*" => vmFile.write("call Math.multiply 2\n")
        case "/" => vmFile.write("call Math.divide 2\n")
        case "&" => vmFile.write("and\n")
        case "|" => vmFile.write("or\n")
        case "&lt;" => vmFile.write("lt\n")
        case "&gt;" => vmFile.write("gt\n")
        case "=" => vmFile.write("eq\n")
        case "&amp;" => vmFile.write("and\n")
      }
    }
  }

  def ParseTerm(): Unit = {

    def isSubRoutineCall : Boolean = tokenList(indexOfTokenList + 1).contains(".") ||
      tokenList(indexOfTokenList + 1).contains("(")

    //<>'(' expression ')'<> || <>unaryOp term<>
    if (getTypeOfToken == "symbol") {
      // <>'(' expression ')'<>
      if (tokenList(indexOfTokenList).contains("(")) {
        goToken() // <>(<>
        ParseExpression()
        goToken() // <>)<>
        return
      }
      //unaryOp term
      if (getContentOfToken == "-" || getContentOfToken == "~") {
        val unaryOp : String = getContentOfToken
        goToken() // <>-<> || <>~<>
        ParseTerm()
        unaryOp match {
          case "-" => vmFile.write("neg\n")
          case "~" => vmFile.write("not\n")
        }
        return
      }
    }
    // <>keyword<>
    if (getTypeOfToken == "keyword" || (SubRoutineKind != "constructor" && getContentOfToken == "this")){
      getContentOfToken match {
        case "true" => vmFile.write("push constant 0\nnot\n")
        case "false" | "null" => vmFile.write("push constant 0\n")
        case "this" => vmFile.write("push pointer 0\n")
      }
      goToken() // <>true<> || <>false<> || <>null<>
      return
    }
    // <>stringConstant<>
    if (getTypeOfToken == "stringConstant"){
      vmFile.write("push constant " + getContentOfToken.length.toString + "\n" +
        "call String.new 1\n")
      for (letter <- getContentOfToken){
        vmFile.write("push constant " + letter.toInt.toString +
          "\ncall String.appendChar 2\n")
      }
      goToken() // <>stringConstant<>
      return
    }
    // <>integerConstant<>
    if (getTypeOfToken == "integerConstant"){
      vmFile.write("push constant " + getContentOfToken + "\n")
      goToken() // <>integerConstant<>
      return
    }
    // <>this<>
    if (getContentOfToken == "this" && SubRoutineKind == "constructor"){
      vmFile.write("push pointer 0\n")
      goToken() // <>this<>
    }
    // <>SubRoutineCall<>
    else if (isSubRoutineCall) {
      ParseSubRoutineCall()
    }
    // <>varName<> || <>varName '[' expression ']'<>
    else{
      val varName : String = getContentOfToken
      if (!isArrayStatement){
        goToken() // <>varName<>
        vmFile.write("push " + symbolTable.kindOf(varName) + " " +
          symbolTable.indexOf(varName) + "\n")
      } else {
        goToken() // <>varName
        goToken() // <>[<>
        ParseExpression()
        vmFile.write("push " + symbolTable.kindOf(varName) + " " +
          symbolTable.indexOf(varName) + "\nadd\npop pointer 1\npush that 0\n")
        goToken() // <>]<>
      }
    }
  }

  def ParseExpressionList(): Unit = {
    if(getContentOfToken != ")"){
      ParseExpression()
      numberOfCallToParams += 1
      while(getContentOfToken == ","){
        goToken() // <>,<>
        ParseExpression()
        numberOfCallToParams += 1
      }
    }
  }

  def isPair(i: Int):Boolean  = {
    if(i % 2 == 0){
      return true
    }
    false
  }

  def getContentOfToken: String = {

    val endResult: Int = tokenList(indexOfTokenList).lastIndexOf('<') - 1
    val startResult: Int = tokenList(indexOfTokenList).indexOf('>') + 1
    tokenList(indexOfTokenList).slice(startResult + 1, endResult)
  }

  def getTypeOfToken: String = {
    val endResult: Int = tokenList(indexOfTokenList).indexOf('>')
    tokenList(indexOfTokenList).slice(1, endResult)
  }

  def goToken() : Unit = indexOfTokenList+=1

  def isArrayStatement : Boolean = tokenList(indexOfTokenList + 1).contains("[")
}


object main {

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def selectVMDir(): String = {
    val chooser = new JFileChooser
    chooser.setCurrentDirectory(new File("C:\\Users\\אלישיב\\Desktop\\מדעי המחשב\\שנה ג סמסטר ב\\קורס עקרונות שפת תוכנה\\תרגיל 5\\tests\\11"))
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

  //Replacing the sensitive chars for xml language
  def removeXMLTags(str: String): String = {
    str.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quet;")
  }


  def main(args: Array[String]): Unit = {


    //Get the working dir
    var pathOfDir  = selectVMDir()
    if (pathOfDir == "NULL") {
      println("Enter path of vm's files dir: ")
      pathOfDir = scala.io.StdIn.readLine()
    }

    //val pathOfDir  = "C:\\Users\\אלישיב\\Desktop\\מדעי המחשב\\שנה ג סמסטר ב\\קורס עקרונות שפת תוכנה\\תרגיל 5\\experiments"
    println(pathOfDir)

    var listOfFiles = getListOfFiles(pathOfDir)

    //Iterating on every jack file in the dir
    for (pathOfFile <- listOfFiles) {
      if (pathOfFile.getPath.endsWith(".jack")) {
        val xmlTFileName = pathOfFile.getPath.split('\\').last
        val xmlTFileNew = new File(pathOfDir + "\\" + xmlTFileName.replace(".jack", "") + "T.xml")
        val xmlTFile: PrintWriter = new PrintWriter(xmlTFileNew)
        xmlTFile.write("<tokens>\n")
        val tokenizer: JackTokenizer = new JackTokenizer(pathOfFile)
        var tokenFlag: TokenType = TokenType.start
        do {
          val token = tokenizer.getToken
          tokenFlag = token._2
          if (tokenFlag != TokenType.eof && tokenFlag != TokenType.error && tokenFlag != TokenType.start){
            xmlTFile.write("<" + tokenFlag.toString + "> " +
              removeXMLTags(token._1.toString) + " </" + tokenFlag.toString + ">\n")
          }
        }
        while (TokenType.eof != tokenFlag && TokenType.error != tokenFlag)
        xmlTFile.write("</tokens>\n")
        xmlTFile.close()
      }
    }

    listOfFiles = getListOfFiles(pathOfDir)
    //Iterating on every T.xml file in the dir
    for (pathOfFile <- listOfFiles) {
      if (pathOfFile.getPath.endsWith("T.xml")) {
        val vmFileName = pathOfFile.getPath.split('\\').last
        val vmFile: PrintWriter = new PrintWriter(
          new File(pathOfDir + "\\" + vmFileName.replace("T.xml", ".vm")))
        val _ : Parsing = new Parsing(pathOfFile, vmFile)
        vmFile.close()
      }
    }
  }
}