import java.io.{File, PrintWriter}
import javax.swing.JFileChooser
import TokenType.TokenType
import scala.io.Source

//Enum for represent the token type
object TokenType extends Enumeration {
  type TokenType = Value
  val keyword, identifier, integerConstant, symbol,
    stringConstant, eof, error, start = Value
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

class Parsing(pathOfXMLTFile : File, xmlFile : PrintWriter){

  var indentation = 0
  var indexOfTokenList=0
  var tokenList:List[String] = _

  //initial call
  Parse()

  def Parse(): Unit = {

    tokenList = Source.fromFile(pathOfXMLTFile).getLines().toList
    indexOfTokenList+=1 //to ignore <>tokens<>
    if(tokenList(indexOfTokenList).contains("class")) {
      ParseClass()
    }
  }

  def ParseClass(): Unit={
    writeOnXML("class")
    indentation +=1
    copyFromOriginal()//<>class<>
    indexOfTokenList +=1
    copyFromOriginal() //<>nameClass<>
    indexOfTokenList +=1
    copyFromOriginal()//<>{<>
    indexOfTokenList +=1
    while(tokenList(indexOfTokenList).contains("static") || tokenList(indexOfTokenList).contains("field")) {
      ParseClassVarDec()
    }
    while(tokenList(indexOfTokenList).contains("constructor") || tokenList(indexOfTokenList).contains("function")|| tokenList(indexOfTokenList).contains("method")) {
      ParseSubRoutineDec()
    }
    copyFromOriginal()//<>"}"<>
    indentation-=1
    writeOnXML("/class")
  }

  def ParseClassVarDec(): Unit={
    writeOnXML("classVarDec")
    indentation+=1
    copyFromOriginal()//<>static|field<>
    indexOfTokenList+=1
    copyFromOriginal()//<>type<>
    indexOfTokenList+=1
    copyFromOriginal()//<>VarName<>
    indexOfTokenList+=1
    while(tokenList(indexOfTokenList).contains(",")){
      copyFromOriginal()//<>","<>
      indexOfTokenList+=1
      copyFromOriginal()//<>VarName<>
      indexOfTokenList+=1
    }
    copyFromOriginal()//<>";"<>
    indexOfTokenList+=1
    indentation-=1
    writeOnXML("/classVarDec")
  }
  def ParseSubRoutineDec(): Unit={
    writeOnXML("subroutineDec")
    indentation+=1
    copyFromOriginal()//<>constructor|function|method<>
    indexOfTokenList+=1
    copyFromOriginal()//<>type|void<>
    indexOfTokenList+=1
    copyFromOriginal()//<>sub routine name<>
    indexOfTokenList+=1
    copyFromOriginal()//<>"("<>
    indexOfTokenList+=1
    ParseParameterList()
    copyFromOriginal()//<>)<>
    indexOfTokenList+=1
    ParseSubRoutineBody()
    indentation-=1
    writeOnXML("/subroutineDec")
  }

  def ParseParameterList(): Unit={
    writeOnXML("parameterList")
    indentation+=1
    if(!tokenList(indexOfTokenList).contains(")")){
      copyFromOriginal()//<>type<>
      indexOfTokenList+=1
      copyFromOriginal()//<>varName<>
      indexOfTokenList+=1
      while(tokenList(indexOfTokenList).contains(",")){
        copyFromOriginal()//<>","<>
        indexOfTokenList+=1
        copyFromOriginal()//<>type<>
        indexOfTokenList+=1
        copyFromOriginal()//<>varName<>
        indexOfTokenList+=1
      }
    }
    indentation-=1
    writeOnXML("/parameterList")
  }

  def ParseSubRoutineBody(): Unit={
    writeOnXML("subroutineBody")
    indentation+=1
    copyFromOriginal()//<>"{"<>
    indexOfTokenList+=1
    while(tokenList(indexOfTokenList).contains("var")){
      ParseVarDec()
    }
    ParseStatements()
    copyFromOriginal()//<>"}"<>
    indexOfTokenList+=1
    indentation-=1
    writeOnXML("/subroutineBody")
  }

  def ParseVarDec(): Unit={
    writeOnXML("varDec")
    indentation+=1
    copyFromOriginal()//<>"var"<>
    indexOfTokenList+=1
    copyFromOriginal()//<>"type"<>
    indexOfTokenList+=1
    copyFromOriginal()//<>"varName"<>
    indexOfTokenList+=1
    while(tokenList(indexOfTokenList).contains(",")){
      copyFromOriginal()//<>","<>
      indexOfTokenList+=1
      copyFromOriginal()//<>"varName"<>
      indexOfTokenList+=1
    }
    copyFromOriginal()//<>";"<>
    indexOfTokenList+=1
    indentation-=1
    writeOnXML("/varDec")
  }

  def ParseStatements(): Unit={
    writeOnXML("statements")
    indentation+=1
    while(tokenList(indexOfTokenList).contains("let") || tokenList(indexOfTokenList).contains("if") || tokenList(indexOfTokenList).contains("while") || tokenList(indexOfTokenList).contains("do") || tokenList(indexOfTokenList).contains("return")) {
      ParseStatement()
    }
    indentation-=1
    writeOnXML("/statements")

  }

  def ParseStatement(): Unit={
    if(tokenList(indexOfTokenList).contains("let")){
      ParseLetStatement()
    }
    else if(tokenList(indexOfTokenList).contains("if")){
      ParseIfStatement()
    }
    else if(tokenList(indexOfTokenList).contains("while")){
      ParseWhileStatement()
    }
    else if(tokenList(indexOfTokenList).contains("do")){
      ParseDoStatement()
    }
    else if(tokenList(indexOfTokenList).contains("return")){
      ParseReturnStatement()
    }
  }

  def ParseLetStatement(): Unit={
    writeOnXML("letStatement")
    indentation+=1
    copyFromOriginal()//<>let<>
    indexOfTokenList+=1
    copyFromOriginal()//<>varName<>
    indexOfTokenList+=1
    if(tokenList(indexOfTokenList).contains("[")){
      copyFromOriginal()//<>[<>
      indexOfTokenList+=1
      ParseExpression()
      copyFromOriginal()//<>]<>
      indexOfTokenList+=1
    }
    copyFromOriginal()//<>"="<>
    indexOfTokenList+=1
    ParseExpression()
    copyFromOriginal()//<>";"<>
    indexOfTokenList+=1
    indentation-=1
    writeOnXML("/letStatement")
  }

  def ParseIfStatement(): Unit={
    writeOnXML("ifStatement")
    indentation+=1
    copyFromOriginal()//<>if<>
    indexOfTokenList+=1
    copyFromOriginal()//<>"("<>
    indexOfTokenList+=1
    ParseExpression()
    copyFromOriginal()//<>")"<>
    indexOfTokenList+=1
    copyFromOriginal()//<>"{"<>
    indexOfTokenList+=1
    ParseStatements()
    copyFromOriginal()//<>"}"<>
    indexOfTokenList+=1
    if(tokenList(indexOfTokenList).contains("else")){
      copyFromOriginal()//<>else<>
      indexOfTokenList+=1
      copyFromOriginal()//<>"{"<>
      indexOfTokenList+=1
      ParseStatements()
      copyFromOriginal()//<>"}"<>
      indexOfTokenList+=1
    }
    indentation-=1
    writeOnXML("/ifStatement")
  }

  def ParseWhileStatement(): Unit={
    writeOnXML("whileStatement")
    indentation+=1
    copyFromOriginal()//<>while<>
    indexOfTokenList+=1
    copyFromOriginal()//<>"("<>
    indexOfTokenList+=1
    ParseExpression()
    copyFromOriginal()//<>")"<>
    indexOfTokenList+=1
    copyFromOriginal()//<>"{"<>
    indexOfTokenList+=1
    ParseStatements()
    copyFromOriginal()//<>"}"<>
    indexOfTokenList+=1
    indentation-=1
    writeOnXML("/whileStatement")
  }

  def ParseDoStatement(): Unit={
    writeOnXML("doStatement")
    indentation+=1
    copyFromOriginal()//<>do<>
    indexOfTokenList+=1
    copyFromOriginal()//<>SubRoutineName || className || varName<>
    indexOfTokenList+=1
    if(tokenList(indexOfTokenList).contains("(")){
      copyFromOriginal()//<>"("<>
      indexOfTokenList+=1
      ParseExpressionList()
      copyFromOriginal()//<>")"<>
      indexOfTokenList+=1
    }
    else if(tokenList(indexOfTokenList).contains(".")){
      copyFromOriginal()//<>"."<>
      indexOfTokenList+=1
      copyFromOriginal()//<>"SubRoutineName"<>
      indexOfTokenList+=1
      copyFromOriginal()//<>"("<>
      indexOfTokenList+=1
      ParseExpressionList()
      copyFromOriginal()//<>")"<>
      indexOfTokenList+=1
    }
    copyFromOriginal()//<>";"<>
    indexOfTokenList+=1
    indentation-=1
    writeOnXML("/doStatement")
  }

  def ParseReturnStatement(): Unit={
    writeOnXML("returnStatement")
    indentation+=1
    copyFromOriginal()//<>return<>
    indexOfTokenList+=1
    if(!tokenList(indexOfTokenList).contains(";")){
      ParseExpression()
    }
    copyFromOriginal()//<>";"<>
    indexOfTokenList+=1
    indentation-=1
    writeOnXML("/returnStatement")
  }

  def ParseExpression(): Unit={
    writeOnXML("expression")
    indentation+=1
    ParseTerm()
    while(tokenList(indexOfTokenList).contains("+") || tokenList(indexOfTokenList).contains("-")
      ||tokenList(indexOfTokenList).contains("*") || isPair(tokenList(indexOfTokenList).count(_=='/'))
      ||tokenList(indexOfTokenList).contains("&") ||tokenList(indexOfTokenList).contains("|")
      || !(isPair(tokenList(indexOfTokenList).count(_=='<'))) || !(isPair(tokenList(indexOfTokenList).count(_=='>')))
      ||tokenList(indexOfTokenList).contains("=")){
      copyFromOriginal() //<>all the options above<>
      indexOfTokenList += 1
      ParseTerm()
    }
    indentation-=1
    writeOnXML("/expression")
  }

  def ParseTerm(): Unit = {
    writeOnXML("term")
    indentation+=1
    if(tokenList(indexOfTokenList).contains("(")) {
      copyFromOriginal() //<>(<>
      indexOfTokenList += 1
      ParseExpression()
      copyFromOriginal() //<>)<>
      indexOfTokenList += 1
    }
    else if(tokenList(indexOfTokenList).contains("-") || tokenList(indexOfTokenList).contains("~")){
      copyFromOriginal() //<>-<> || <>~<>
      indexOfTokenList += 1
      ParseTerm()
    }
    else{
      copyFromOriginal() //a lot of options
      indexOfTokenList += 1
      if(tokenList(indexOfTokenList).contains("[")) {
        copyFromOriginal()//<>"["<>
        indexOfTokenList+=1
        ParseExpression()
        copyFromOriginal()//<>"]"<>
        indexOfTokenList+=1
      }
      else if(tokenList(indexOfTokenList).contains("(")){
        copyFromOriginal()//<>"("<>
        indexOfTokenList+=1
        ParseExpressionList()
        copyFromOriginal()//<>")"<>
        indexOfTokenList+=1
      }
      else if(tokenList(indexOfTokenList).contains(".")){
        copyFromOriginal()//<>"."<>
        indexOfTokenList+=1
        copyFromOriginal()//<>"SubRoutineName"<>
        indexOfTokenList+=1
        copyFromOriginal()//<>"("<>
        indexOfTokenList+=1
        ParseExpressionList()
        copyFromOriginal()//<>")"<>
        indexOfTokenList+=1
      }
    }
    indentation-=1
    writeOnXML("/term")
  }

  def ParseExpressionList(): Unit={
    writeOnXML("expressionList")
    indentation+=1
    if(!tokenList(indexOfTokenList).contains(")")){
      ParseExpression()
      while(tokenList(indexOfTokenList).contains(",")){
        copyFromOriginal()//<>","<>
        indexOfTokenList+=1
        ParseExpression()
      }
    }
    indentation-=1
    writeOnXML("/expressionList")
  }

  def writeOnXML(tok : String): Unit={
    xmlFile.write("  " * indentation + "<" + tok + ">\n")
  }

  def copyFromOriginal(): Unit={
    xmlFile.write("  " * indentation + tokenList(indexOfTokenList) + "\n")
  }

  def isPair(i: Int):Boolean  = {
    if(i % 2 == 0){
      return true
    }
    false
  }
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
    chooser.setCurrentDirectory(new File("C:\\Users\\אלישיב\\Desktop\\מדעי המחשב\\שנה ג סמסטר ב\\קורס עקרונות שפת תוכנה\\תרגיל 4\\projects\\10"))
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
    println(pathOfDir)

    val listOfFiles = getListOfFiles(pathOfDir)

    //Iterating on every jack file in the dir
    for (pathOfFile <- listOfFiles) {
      if (pathOfFile.getPath.endsWith(".jack")) {
        var xmlTFileName = pathOfFile.getPath.split('\\').last
        var xmlTFile: PrintWriter = new PrintWriter(
          new File(pathOfDir + "\\" + xmlTFileName.replace(".jack", "") + "T.xml"))
        xmlTFile.write("<tokens>\n")
        val tokenizer: JackTokenizer = new JackTokenizer(pathOfFile)
        var tokenFlag: TokenType = TokenType.start
        do {
          val token = tokenizer.getToken
          tokenFlag = token._2
          if (tokenFlag != TokenType.eof && tokenFlag != TokenType.error && tokenFlag != TokenType.start){
            xmlTFile.write("<" + token._2.toString + "> " +
              removeXMLTags(token._1.toString) + " </" + token._2.toString + ">\n")
          }
        }
        while (TokenType.eof != tokenFlag && TokenType.error != tokenFlag)
        xmlTFile.write("</tokens>\n")
        xmlTFile.close()
      }
    }

    //Iterating on every T.xml file in the dir
    for (pathOfFile <- listOfFiles) {
      if (pathOfFile.getPath.endsWith("T.xml")) {
        val xmlFileName = pathOfFile.getPath.split('\\').last
        val xmlFile: PrintWriter = new PrintWriter(
          new File(pathOfDir + "\\" + xmlFileName.replace("T.xml", ".xml")))
        val parsing: Parsing = new Parsing(pathOfFile, xmlFile)
        xmlFile.close()
      }
    }
  }
}