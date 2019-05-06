/* The following code was generated by JFlex 1.7.0 */

 /** JFlex lexical analyzer for PL0.
 * The input to JFlex consists of three sections:
 * - user code to be included directly in the generated class,
 * - options and definitions for JFlex, and
 * - lexical rules defining the tokens.
 * These sections are separated by lines containing just "%%"
 */

/* --------------------------Usercode Section------------------------ */

package parse;

import java_cup.runtime.*;
import source.ErrorHandler;


/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.7.0
 * from the specification file <tt>PL0.flex</tt>
 */
public class Lexer implements java_cup.runtime.Scanner {

  /** This character denotes the end of file */
  public static final int YYEOF = -1;

  /** initial size of the lookahead buffer */
  private static final int ZZ_BUFFERSIZE = 16384;

  /** lexical states */
  public static final int YYINITIAL = 0;

  /**
   * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
   * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
   *                  at the beginning of a line
   * l is of the form l = 2*k, k a non negative integer
   */
  private static final int ZZ_LEXSTATE[] = { 
     0, 0
  };

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\11\0\1\4\1\2\1\56\1\3\1\1\22\0\1\4\1\20\6\0"+
    "\1\7\1\10\1\16\1\14\1\24\1\15\1\23\1\17\12\5\1\12"+
    "\1\11\1\21\1\13\1\22\2\0\32\6\1\30\1\0\1\31\1\25"+
    "\2\0\1\40\1\32\1\37\1\45\1\33\1\46\1\34\1\53\1\35"+
    "\2\6\1\41\1\6\1\36\1\42\1\50\1\6\1\51\1\43\1\44"+
    "\1\52\1\55\1\47\1\6\1\54\1\6\1\26\1\0\1\27\7\0"+
    "\1\56\u1fa2\0\1\56\1\56\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\udfe6\0";

  /** 
   * Translates characters to character classes
   */
  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

  /** 
   * Translates DFA states to action switch labels.
   */
  private static final int [] ZZ_ACTION = zzUnpackAction();

  private static final String ZZ_ACTION_PACKED_0 =
    "\1\0\1\1\2\2\1\3\1\4\1\5\1\6\1\7"+
    "\1\10\1\11\1\12\1\13\1\14\1\15\1\1\1\16"+
    "\1\17\1\20\1\21\1\22\1\23\1\24\1\25\1\26"+
    "\13\4\1\27\1\30\1\31\1\32\1\33\1\34\3\4"+
    "\1\35\5\4\1\36\6\4\1\37\1\4\1\40\11\4"+
    "\1\41\1\4\1\42\1\43\1\4\1\44\1\45\4\4"+
    "\1\46\1\47\1\50\1\51\1\52\3\4\1\53\2\4"+
    "\1\54";

  private static int [] zzUnpackAction() {
    int [] result = new int[93];
    int offset = 0;
    offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAction(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /** 
   * Translates a state to a row index in the transition table
   */
  private static final int [] ZZ_ROWMAP = zzUnpackRowMap();

  private static final String ZZ_ROWMAP_PACKED_0 =
    "\0\0\0\57\0\136\0\57\0\215\0\274\0\57\0\57"+
    "\0\57\0\353\0\57\0\57\0\57\0\57\0\u011a\0\u0149"+
    "\0\u0178\0\u01a7\0\u01d6\0\57\0\57\0\57\0\57\0\57"+
    "\0\57\0\u0205\0\u0234\0\u0263\0\u0292\0\u02c1\0\u02f0\0\u031f"+
    "\0\u034e\0\u037d\0\u03ac\0\u03db\0\57\0\u040a\0\57\0\57"+
    "\0\57\0\57\0\u0439\0\u0468\0\u0497\0\274\0\u04c6\0\u04f5"+
    "\0\u0524\0\u0553\0\u0582\0\274\0\u05b1\0\u05e0\0\u060f\0\u063e"+
    "\0\u066d\0\u069c\0\274\0\u06cb\0\274\0\u06fa\0\u0729\0\u0758"+
    "\0\u0787\0\u07b6\0\u07e5\0\u0814\0\u0843\0\u0872\0\274\0\u08a1"+
    "\0\274\0\274\0\u08d0\0\274\0\274\0\u08ff\0\u092e\0\u095d"+
    "\0\u098c\0\274\0\274\0\274\0\274\0\274\0\u09bb\0\u09ea"+
    "\0\u0a19\0\274\0\u0a48\0\u0a77\0\274";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[93];
    int offset = 0;
    offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackRowMap(String packed, int offset, int [] result) {
    int i = 0;  /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int high = packed.charAt(i++) << 16;
      result[j++] = high | packed.charAt(i++);
    }
    return j;
  }

  /** 
   * The transition table of the DFA
   */
  private static final int [] ZZ_TRANS = zzUnpackTrans();

  private static final String ZZ_TRANS_PACKED_0 =
    "\1\2\1\3\3\4\1\5\1\6\1\7\1\10\1\11"+
    "\1\12\1\13\1\14\1\15\1\16\1\17\1\20\1\21"+
    "\1\22\1\23\1\24\1\25\1\26\1\27\1\30\1\31"+
    "\1\32\1\33\1\6\1\34\1\35\1\36\4\6\1\37"+
    "\1\40\1\6\1\41\1\42\1\43\3\6\1\44\62\0"+
    "\1\4\61\0\1\5\56\0\2\6\23\0\24\6\14\0"+
    "\1\45\62\0\1\46\52\0\1\47\56\0\1\50\56\0"+
    "\1\51\66\0\1\52\40\0\2\6\23\0\1\6\1\53"+
    "\22\6\6\0\2\6\23\0\4\6\1\54\2\6\1\55"+
    "\14\6\6\0\2\6\23\0\14\6\1\56\7\6\6\0"+
    "\2\6\23\0\1\6\1\57\22\6\6\0\2\6\23\0"+
    "\6\6\1\60\1\6\1\61\13\6\6\0\2\6\23\0"+
    "\21\6\1\62\1\63\1\6\6\0\2\6\23\0\10\6"+
    "\1\64\13\6\6\0\2\6\23\0\17\6\1\65\1\6"+
    "\1\66\2\6\6\0\2\6\23\0\17\6\1\67\4\6"+
    "\6\0\2\6\23\0\1\6\1\70\22\6\6\0\2\6"+
    "\23\0\6\6\1\71\15\6\1\0\1\46\3\0\53\46"+
    "\5\0\2\6\23\0\2\6\1\72\21\6\6\0\2\6"+
    "\23\0\13\6\1\73\10\6\6\0\2\6\23\0\11\6"+
    "\1\74\12\6\6\0\2\6\23\0\15\6\1\75\6\6"+
    "\6\0\2\6\23\0\7\6\1\76\14\6\6\0\2\6"+
    "\23\0\4\6\1\77\17\6\6\0\2\6\23\0\1\6"+
    "\1\100\22\6\6\0\2\6\23\0\16\6\1\101\5\6"+
    "\6\0\2\6\23\0\3\6\1\102\20\6\6\0\2\6"+
    "\23\0\3\6\1\103\20\6\6\0\2\6\23\0\10\6"+
    "\1\104\13\6\6\0\2\6\23\0\5\6\1\105\1\106"+
    "\15\6\6\0\2\6\23\0\17\6\1\107\4\6\6\0"+
    "\2\6\23\0\3\6\1\110\20\6\6\0\2\6\23\0"+
    "\1\6\1\111\22\6\6\0\2\6\23\0\7\6\1\112"+
    "\14\6\6\0\2\6\23\0\11\6\1\113\12\6\6\0"+
    "\2\6\23\0\4\6\1\114\17\6\6\0\2\6\23\0"+
    "\1\6\1\115\22\6\6\0\2\6\23\0\12\6\1\116"+
    "\11\6\6\0\2\6\23\0\7\6\1\117\14\6\6\0"+
    "\2\6\23\0\5\6\1\120\16\6\6\0\2\6\23\0"+
    "\10\6\1\121\13\6\6\0\2\6\23\0\13\6\1\122"+
    "\10\6\6\0\2\6\23\0\4\6\1\123\17\6\6\0"+
    "\2\6\23\0\12\6\1\124\11\6\6\0\2\6\23\0"+
    "\1\6\1\125\22\6\6\0\2\6\23\0\1\6\1\126"+
    "\22\6\6\0\2\6\23\0\1\6\1\127\22\6\6\0"+
    "\2\6\23\0\17\6\1\130\4\6\6\0\2\6\23\0"+
    "\13\6\1\131\10\6\6\0\2\6\23\0\13\6\1\132"+
    "\10\6\6\0\2\6\23\0\20\6\1\133\3\6\6\0"+
    "\2\6\23\0\17\6\1\134\4\6\6\0\2\6\23\0"+
    "\1\6\1\135\22\6\1\0";

  private static int [] zzUnpackTrans() {
    int [] result = new int[2726];
    int offset = 0;
    offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackTrans(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      value--;
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /* error codes */
  private static final int ZZ_UNKNOWN_ERROR = 0;
  private static final int ZZ_NO_MATCH = 1;
  private static final int ZZ_PUSHBACK_2BIG = 2;

  /* error messages for the codes above */
  private static final String ZZ_ERROR_MSG[] = {
    "Unknown internal scanner error",
    "Error: could not match input",
    "Error: pushback value was too large"
  };

  /**
   * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
   */
  private static final int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

  private static final String ZZ_ATTRIBUTE_PACKED_0 =
    "\1\0\1\11\1\1\1\11\2\1\3\11\1\1\4\11"+
    "\5\1\6\11\13\1\1\11\1\1\4\11\63\1";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[93];
    int offset = 0;
    offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAttribute(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }

  /** the input device */
  private java.io.Reader zzReader;

  /** the current state of the DFA */
  private int zzState;

  /** the current lexical state */
  private int zzLexicalState = YYINITIAL;

  /** this buffer contains the current text to be matched and is
      the source of the yytext() string */
  private char zzBuffer[] = new char[ZZ_BUFFERSIZE];

  /** the textposition at the last accepting state */
  private int zzMarkedPos;

  /** the current text position in the buffer */
  private int zzCurrentPos;

  /** startRead marks the beginning of the yytext() string in the buffer */
  private int zzStartRead;

  /** endRead marks the last character in the buffer, that has been read
      from input */
  private int zzEndRead;

  /** number of newlines encountered up to the start of the matched text */
  private int yyline;

  /** the number of characters up to the start of the matched text */
  private int yychar;

  /**
   * the number of characters from the last newline up to the start of the 
   * matched text
   */
  private int yycolumn;

  /** 
   * zzAtBOL == true iff the scanner is currently at the beginning of a line
   */
  private boolean zzAtBOL = true;

  /** zzAtEOF == true iff the scanner is at the EOF */
  private boolean zzAtEOF;

  /** denotes if the user-EOF-code has already been executed */
  private boolean zzEOFDone;
  
  /** 
   * The number of occupied positions in zzBuffer beyond zzEndRead.
   * When a lead/high surrogate has been read from the input stream
   * into the final zzBuffer position, this will have a value of 1;
   * otherwise, it will have a value of 0.
   */
  private int zzFinalHighSurrogate = 0;

  /* user code: */
    ComplexSymbolFactory sf;
    public Lexer(java.io.Reader in, ComplexSymbolFactory sf){
        this(in);
        this.sf = sf;
    }

    /** To create a new java_cup.runtime.Symbol.
     * @param kind is an integer code representing the token.
     * Note that CUP and JFlex use integers to represent token kinds.
     */
    private Symbol makeToken(int kind) {
        /* Symbol takes the token kind, and the locations of the
         * leftmost and rightmost characters of the substring of the
         * input file that matched the token. 
         */
        // System.err.println("Token " + yytext() + 
        //   " @ (" + yyline + "," + yycolumn + ")" + kind);
        return sf.newSymbol(CUPToken.terminalNames[kind], kind, 
            new ComplexSymbolFactory.Location(yyline, yycolumn), 
            new ComplexSymbolFactory.Location(yyline, yycolumn + yylength()-1));
    }
    /** Also creates a new java_cup.runtime.Symbol with information
     * about the current token, but this object has a value. 
     * @param kind is an integer code representing the token.
     * @param value is an arbitrary Java Object.
     * Below when tokens such as a NUMBER or IDENTIFIER are 
     * recognised they pass values which are respectively
     * of type Integer and String. The types of these values *must*
     * match their type as declared in the Terminals sections
     * of the CUP specification.
     */
    private Symbol makeToken(int kind, Object value) {
        // System.err.println("Token " + yytext() + " " +kind);
        return sf.newSymbol(CUPToken.terminalNames[kind], kind, 
            new ComplexSymbolFactory.Location(yyline, yycolumn), 
            new ComplexSymbolFactory.Location(yyline, yycolumn + yylength()-1), 
            value);
    }


  /**
   * Creates a new scanner
   *
   * @param   in  the java.io.Reader to read input from.
   */
  public Lexer(java.io.Reader in) {
    this.zzReader = in;
  }


  /** 
   * Unpacks the compressed character translation table.
   *
   * @param packed   the packed character translation table
   * @return         the unpacked character translation table
   */
  private static char [] zzUnpackCMap(String packed) {
    char [] map = new char[0x110000];
    int i = 0;  /* index in packed string  */
    int j = 0;  /* index in unpacked array */
    while (i < 162) {
      int  count = packed.charAt(i++);
      char value = packed.charAt(i++);
      do map[j++] = value; while (--count > 0);
    }
    return map;
  }


  /**
   * Refills the input buffer.
   *
   * @return      <code>false</code>, iff there was new input.
   * 
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  private boolean zzRefill() throws java.io.IOException {

    /* first: make room (if you can) */
    if (zzStartRead > 0) {
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
      System.arraycopy(zzBuffer, zzStartRead,
                       zzBuffer, 0,
                       zzEndRead-zzStartRead);

      /* translate stored positions */
      zzEndRead-= zzStartRead;
      zzCurrentPos-= zzStartRead;
      zzMarkedPos-= zzStartRead;
      zzStartRead = 0;
    }

    /* is the buffer big enough? */
    if (zzCurrentPos >= zzBuffer.length - zzFinalHighSurrogate) {
      /* if not: blow it up */
      char newBuffer[] = new char[zzBuffer.length*2];
      System.arraycopy(zzBuffer, 0, newBuffer, 0, zzBuffer.length);
      zzBuffer = newBuffer;
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
    }

    /* fill the buffer with new input */
    int requested = zzBuffer.length - zzEndRead;
    int numRead = zzReader.read(zzBuffer, zzEndRead, requested);

    /* not supposed to occur according to specification of java.io.Reader */
    if (numRead == 0) {
      throw new java.io.IOException("Reader returned 0 characters. See JFlex examples for workaround.");
    }
    if (numRead > 0) {
      zzEndRead += numRead;
      /* If numRead == requested, we might have requested to few chars to
         encode a full Unicode character. We assume that a Reader would
         otherwise never return half characters. */
      if (numRead == requested) {
        if (Character.isHighSurrogate(zzBuffer[zzEndRead - 1])) {
          --zzEndRead;
          zzFinalHighSurrogate = 1;
        }
      }
      /* potentially more input available */
      return false;
    }

    /* numRead < 0 ==> end of stream */
    return true;
  }

    
  /**
   * Closes the input stream.
   */
  public final void yyclose() throws java.io.IOException {
    zzAtEOF = true;            /* indicate end of file */
    zzEndRead = zzStartRead;  /* invalidate buffer    */

    if (zzReader != null)
      zzReader.close();
  }


  /**
   * Resets the scanner to read from a new input stream.
   * Does not close the old reader.
   *
   * All internal variables are reset, the old input stream 
   * <b>cannot</b> be reused (internal buffer is discarded and lost).
   * Lexical state is set to <tt>ZZ_INITIAL</tt>.
   *
   * Internal scan buffer is resized down to its initial length, if it has grown.
   *
   * @param reader   the new input stream 
   */
  public final void yyreset(java.io.Reader reader) {
    zzReader = reader;
    zzAtBOL  = true;
    zzAtEOF  = false;
    zzEOFDone = false;
    zzEndRead = zzStartRead = 0;
    zzCurrentPos = zzMarkedPos = 0;
    zzFinalHighSurrogate = 0;
    yyline = yychar = yycolumn = 0;
    zzLexicalState = YYINITIAL;
    if (zzBuffer.length > ZZ_BUFFERSIZE)
      zzBuffer = new char[ZZ_BUFFERSIZE];
  }


  /**
   * Returns the current lexical state.
   */
  public final int yystate() {
    return zzLexicalState;
  }


  /**
   * Enters a new lexical state
   *
   * @param newState the new lexical state
   */
  public final void yybegin(int newState) {
    zzLexicalState = newState;
  }


  /**
   * Returns the text matched by the current regular expression.
   */
  public final String yytext() {
    return new String( zzBuffer, zzStartRead, zzMarkedPos-zzStartRead );
  }


  /**
   * Returns the character at position <tt>pos</tt> from the 
   * matched text. 
   * 
   * It is equivalent to yytext().charAt(pos), but faster
   *
   * @param pos the position of the character to fetch. 
   *            A value from 0 to yylength()-1.
   *
   * @return the character at position pos
   */
  public final char yycharat(int pos) {
    return zzBuffer[zzStartRead+pos];
  }


  /**
   * Returns the length of the matched text region.
   */
  public final int yylength() {
    return zzMarkedPos-zzStartRead;
  }


  /**
   * Reports an error that occured while scanning.
   *
   * In a wellformed scanner (no or only correct usage of 
   * yypushback(int) and a match-all fallback rule) this method 
   * will only be called with things that "Can't Possibly Happen".
   * If this method is called, something is seriously wrong
   * (e.g. a JFlex bug producing a faulty scanner etc.).
   *
   * Usual syntax/scanner level error handling should be done
   * in error fallback rules.
   *
   * @param   errorCode  the code of the errormessage to display
   */
  private void zzScanError(int errorCode) {
    String message;
    try {
      message = ZZ_ERROR_MSG[errorCode];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
    }

    throw new Error(message);
  } 


  /**
   * Pushes the specified amount of characters back into the input stream.
   *
   * They will be read again by then next call of the scanning method
   *
   * @param number  the number of characters to be read again.
   *                This number must not be greater than yylength()!
   */
  public void yypushback(int number)  {
    if ( number > yylength() )
      zzScanError(ZZ_PUSHBACK_2BIG);

    zzMarkedPos -= number;
  }


  /**
   * Contains user EOF-code, which will be executed exactly once,
   * when the end of file is reached
   */
  private void zzDoEOF() throws java.io.IOException {
    if (!zzEOFDone) {
      zzEOFDone = true;
      yyclose();
    }
  }


  /**
   * Resumes scanning until the next regular expression is matched,
   * the end of input is encountered or an I/O-Error occurs.
   *
   * @return      the next token
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  public java_cup.runtime.Symbol next_token() throws java.io.IOException {
    int zzInput;
    int zzAction;

    // cached fields:
    int zzCurrentPosL;
    int zzMarkedPosL;
    int zzEndReadL = zzEndRead;
    char [] zzBufferL = zzBuffer;
    char [] zzCMapL = ZZ_CMAP;

    int [] zzTransL = ZZ_TRANS;
    int [] zzRowMapL = ZZ_ROWMAP;
    int [] zzAttrL = ZZ_ATTRIBUTE;

    while (true) {
      zzMarkedPosL = zzMarkedPos;

      boolean zzR = false;
      int zzCh;
      int zzCharCount;
      for (zzCurrentPosL = zzStartRead  ;
           zzCurrentPosL < zzMarkedPosL ;
           zzCurrentPosL += zzCharCount ) {
        zzCh = Character.codePointAt(zzBufferL, zzCurrentPosL, zzMarkedPosL);
        zzCharCount = Character.charCount(zzCh);
        switch (zzCh) {
        case '\u000B':  // fall through
        case '\u000C':  // fall through
        case '\u0085':  // fall through
        case '\u2028':  // fall through
        case '\u2029':
          yyline++;
          yycolumn = 0;
          zzR = false;
          break;
        case '\r':
          yyline++;
          yycolumn = 0;
          zzR = true;
          break;
        case '\n':
          if (zzR)
            zzR = false;
          else {
            yyline++;
            yycolumn = 0;
          }
          break;
        default:
          zzR = false;
          yycolumn += zzCharCount;
        }
      }

      if (zzR) {
        // peek one character ahead if it is \n (if we have counted one line too much)
        boolean zzPeek;
        if (zzMarkedPosL < zzEndReadL)
          zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        else if (zzAtEOF)
          zzPeek = false;
        else {
          boolean eof = zzRefill();
          zzEndReadL = zzEndRead;
          zzMarkedPosL = zzMarkedPos;
          zzBufferL = zzBuffer;
          if (eof) 
            zzPeek = false;
          else 
            zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        }
        if (zzPeek) yyline--;
      }
      zzAction = -1;

      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
  
      zzState = ZZ_LEXSTATE[zzLexicalState];

      // set up zzAction for empty match case:
      int zzAttributes = zzAttrL[zzState];
      if ( (zzAttributes & 1) == 1 ) {
        zzAction = zzState;
      }


      zzForAction: {
        while (true) {
    
          if (zzCurrentPosL < zzEndReadL) {
            zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
            zzCurrentPosL += Character.charCount(zzInput);
          }
          else if (zzAtEOF) {
            zzInput = YYEOF;
            break zzForAction;
          }
          else {
            // store back cached positions
            zzCurrentPos  = zzCurrentPosL;
            zzMarkedPos   = zzMarkedPosL;
            boolean eof = zzRefill();
            // get translated positions and possibly new buffer
            zzCurrentPosL  = zzCurrentPos;
            zzMarkedPosL   = zzMarkedPos;
            zzBufferL      = zzBuffer;
            zzEndReadL     = zzEndRead;
            if (eof) {
              zzInput = YYEOF;
              break zzForAction;
            }
            else {
              zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
              zzCurrentPosL += Character.charCount(zzInput);
            }
          }
          int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];
          if (zzNext == -1) break zzForAction;
          zzState = zzNext;

          zzAttributes = zzAttrL[zzState];
          if ( (zzAttributes & 1) == 1 ) {
            zzAction = zzState;
            zzMarkedPosL = zzCurrentPosL;
            if ( (zzAttributes & 8) == 8 ) break zzForAction;
          }

        }
      }

      // store back cached position
      zzMarkedPos = zzMarkedPosL;

      if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
        zzAtEOF = true;
            zzDoEOF();
          {     return makeToken(CUPToken.EOF);
 }
      }
      else {
        switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
          case 1: 
            { return makeToken(CUPToken.ILLEGAL);
            } 
            // fall through
          case 45: break;
          case 2: 
            { /* ignore white space */
            } 
            // fall through
          case 46: break;
          case 3: 
            { int value = 0x80808080; // Nonsense value
      try {
          value = Integer.parseInt(yytext());
      } catch(NumberFormatException e) { 
          /* Can only happen if the number is too big */
          ErrorHandler.getErrorHandler().error(
            "integer too large", 
            new ComplexSymbolFactory.Location(yyline, yycolumn));
      }
      return makeToken(CUPToken.NUMBER, new Integer(value));
            } 
            // fall through
          case 47: break;
          case 4: 
            { return makeToken(CUPToken.IDENTIFIER, yytext());
            } 
            // fall through
          case 48: break;
          case 5: 
            { return makeToken(CUPToken.LPAREN);
            } 
            // fall through
          case 49: break;
          case 6: 
            { return makeToken(CUPToken.RPAREN);
            } 
            // fall through
          case 50: break;
          case 7: 
            { return makeToken(CUPToken.SEMICOLON);
            } 
            // fall through
          case 51: break;
          case 8: 
            { return makeToken(CUPToken.COLON);
            } 
            // fall through
          case 52: break;
          case 9: 
            { return makeToken(CUPToken.EQUALS);
            } 
            // fall through
          case 53: break;
          case 10: 
            { return makeToken(CUPToken.PLUS);
            } 
            // fall through
          case 54: break;
          case 11: 
            { return makeToken(CUPToken.MINUS);
            } 
            // fall through
          case 55: break;
          case 12: 
            { return makeToken(CUPToken.TIMES);
            } 
            // fall through
          case 56: break;
          case 13: 
            { return makeToken(CUPToken.DIVIDE);
            } 
            // fall through
          case 57: break;
          case 14: 
            { return makeToken(CUPToken.LESS);
            } 
            // fall through
          case 58: break;
          case 15: 
            { return makeToken(CUPToken.GREATER);
            } 
            // fall through
          case 59: break;
          case 16: 
            { return makeToken(CUPToken.PERIOD);
            } 
            // fall through
          case 60: break;
          case 17: 
            { return makeToken(CUPToken.COMMA);
            } 
            // fall through
          case 61: break;
          case 18: 
            { return makeToken(CUPToken.POINTER);
            } 
            // fall through
          case 62: break;
          case 19: 
            { return makeToken(CUPToken.LCURLY);
            } 
            // fall through
          case 63: break;
          case 20: 
            { return makeToken(CUPToken.RCURLY);
            } 
            // fall through
          case 64: break;
          case 21: 
            { return makeToken(CUPToken.LBRACKET);
            } 
            // fall through
          case 65: break;
          case 22: 
            { return makeToken(CUPToken.RBRACKET);
            } 
            // fall through
          case 66: break;
          case 23: 
            { return makeToken(CUPToken.ASSIGN);
            } 
            // fall through
          case 67: break;
          case 24: 
            { /* ignore comment - an empty action causes the lexical analyser
       * to skip the matched characters in the input and then start
       * scanning for a token from the next character. */
            } 
            // fall through
          case 68: break;
          case 25: 
            { return makeToken(CUPToken.NEQUALS);
            } 
            // fall through
          case 69: break;
          case 26: 
            { return makeToken(CUPToken.LEQUALS);
            } 
            // fall through
          case 70: break;
          case 27: 
            { return makeToken(CUPToken.GEQUALS);
            } 
            // fall through
          case 71: break;
          case 28: 
            { return makeToken(CUPToken.RANGE);
            } 
            // fall through
          case 72: break;
          case 29: 
            { return makeToken(CUPToken.KW_IF);
            } 
            // fall through
          case 73: break;
          case 30: 
            { return makeToken(CUPToken.KW_DO);
            } 
            // fall through
          case 74: break;
          case 31: 
            { return makeToken(CUPToken.KW_END);
            } 
            // fall through
          case 75: break;
          case 32: 
            { return makeToken(CUPToken.KW_NEW);
            } 
            // fall through
          case 76: break;
          case 33: 
            { return makeToken(CUPToken.KW_VAR);
            } 
            // fall through
          case 77: break;
          case 34: 
            { return makeToken(CUPToken.KW_ELSE);
            } 
            // fall through
          case 78: break;
          case 35: 
            { return makeToken(CUPToken.KW_CALL);
            } 
            // fall through
          case 79: break;
          case 36: 
            { return makeToken(CUPToken.KW_THEN);
            } 
            // fall through
          case 80: break;
          case 37: 
            { return makeToken(CUPToken.KW_TYPE);
            } 
            // fall through
          case 81: break;
          case 38: 
            { return makeToken(CUPToken.KW_READ);
            } 
            // fall through
          case 82: break;
          case 39: 
            { return makeToken(CUPToken.KW_BEGIN);
            } 
            // fall through
          case 83: break;
          case 40: 
            { return makeToken(CUPToken.KW_CONST);
            } 
            // fall through
          case 84: break;
          case 41: 
            { return makeToken(CUPToken.KW_WRITE);
            } 
            // fall through
          case 85: break;
          case 42: 
            { return makeToken(CUPToken.KW_WHILE);
            } 
            // fall through
          case 86: break;
          case 43: 
            { return makeToken(CUPToken.KW_RECORD);
            } 
            // fall through
          case 87: break;
          case 44: 
            { return makeToken(CUPToken.KW_PROCEDURE);
            } 
            // fall through
          case 88: break;
          default:
            zzScanError(ZZ_NO_MATCH);
        }
      }
    }
  }


}
