[#ftl strict_vars=true]
[#--
/* Copyright (c) 2008-2020 Jonathan Revusky, revusky@javacc.com
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright notices,
 *       this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name Jonathan Revusky, Sun Microsystems, Inc.
 *       nor the names of any contributors may be used to endorse 
 *       or promote products derived from this software without specific prior written 
 *       permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */
 --]

 [#var lexerData=grammar.lexerData]
 [#var utils=grammar.utils]
 [#var tokenCount=lexerData.tokenCount]
 [#var numLexicalStates=lexerData.lexicalStates?size]
 [#var multipleLexicalStates = numLexicalStates>1]

[#var MAX_INT=2147483647]

   private int[] jjemptyLineNo = new int[${numLexicalStates}];
   private int[] jjemptyColNo = new int[${numLexicalStates}];
   private boolean[] jjbeenHere = new boolean[${numLexicalStates}];
  
  
  private int jjmatchedPos;
  //FIXME,should be an enum.
  private int jjmatchedKind;
  private TokenType matchedType;
  private String inputSource = "input";

 [#macro BitSetFromLongArray bitSet]
      BitSet.valueOf(new long[] {
          [#list bitSet.toLongArray() as long]
             ${utils.toHexStringL(long)}
             [#if long_has_next],[/#if]
          [/#list]
      })
[/#macro]
  
    static private final BitSet tokenSet = ${BitSetFromLongArray(lexerData.tokenSet)},
                                specialSet = ${BitSetFromLongArray(lexerData.specialSet)},
                                skipSet = ${BitSetFromLongArray(lexerData.skipSet)},
                                moreSet = ${BitSetFromLongArray(lexerData.moreSet)};

  
    private final StringBuilder image = new StringBuilder();
    private int curChar, matchedCharsLength;
    
    private Token generateEOF() {
      if (trace_enabled) LOGGER.info("Returning the <EOF> token.");
	    jjmatchedKind = 0;
      matchedType = TokenType.EOF;
      Token eof = jjFillToken();
      tokenLexicalActions();
[#list grammar.lexerTokenHooks as tokenHookMethodName]
      [#if tokenHookMethodName != "CommonTokenAction"]
         eof =
      [/#if]
      ${tokenHookMethodName}(eof);
[/#list]
      return eof;
    }
  
  [#--  Need to figure out how to simplify this --]
  private Token nextToken() {
    Token matchedToken;
    int curPos = 0;

    EOFLoop :
    while (true) {
        curChar = input_stream.beginToken();
        if (curChar == -1) {
           return generateEOF();
        }
       image.setLength(0);
       matchedCharsLength = 0;
[#if lexerData.hasMore]
       while (true) {
[/#if]
    [#-- this also sets up the start state of the nfa --]
[#if multipleLexicalStates]
       switch(lexicalState) {
[/#if]
    
[#list lexerData.lexicalStates as lexicalState]
    [#if multipleLexicalStates]
            case ${lexicalState.name} : 
    [/#if]
    [@SkipSingles lexicalState /]
    jjmatchedKind = 0x7FFFFFFF;
    matchedType = null;
    jjmatchedPos = 0;
    [#var debugOutput]
    [#set debugOutput]
        [#if multipleLexicalStates]
            "<" + lexicalState + ">" + 
        [/#if]
        [#-- REVISIT--]
        "Current character : " + addEscapes(String.valueOf(curChar)) + " (" + curChar + ") " +
        "at line " + input_stream.getEndLine() + " column " + input_stream.getEndColumn()
    [/#set]
    if (trace_enabled) LOGGER.info(${debugOutput?trim}); 
    curPos = jjMoveStringLiteralDfa0_${lexicalState.name}();
    [#if multipleLexicalStates]
        break;
    [/#if]
[/#list]
  [#if multipleLexicalStates]
      }
  [/#if]
  if (jjmatchedKind != 0x7FFFFFFF) { 
      if (jjmatchedPos + 1 < curPos) {
        if (trace_enabled) LOGGER.info("   Putting back " + (curPos - jjmatchedPos - 1) + " characters into the input stream.");
        input_stream.backup(curPos - jjmatchedPos - 1);
      }
       if (trace_enabled) LOGGER.info("****** FOUND A " + tokenImage[jjmatchedKind] + " MATCH ("
          + addEscapes(input_stream.getSuffix(jjmatchedPos + 2)) + ") ******\n");
 
       if (tokenSet.get(jjmatchedKind) || specialSet.get(jjmatchedKind)) {

         matchedToken = jjFillToken();
 [#list grammar.lexerTokenHooks as tokenHookMethodName]
      [#if tokenHookMethodName = "CommonTokenAction"]
         ${tokenHookMethodName}(matchedToken);
      [#else]
         matchedToken = ${tokenHookMethodName}(matchedToken);
      [/#if]
[/#list]
      tokenLexicalActions();
      jjmatchedKind = matchedToken.getType().ordinal();
 
 [#if multipleLexicalStates]
      if (newLexicalStates[jjmatchedKind] != null) {
          switchTo(newLexicalStates[jjmatchedKind]);
      }
 [/#if]
      matchedToken.setUnparsed(specialSet.get(jjmatchedKind));
      return matchedToken;

     }
         [#if lexerData.hasSkip || lexerData.hasSpecial]
            [#if lexerData.hasMore]
          else if (skipSet.get(jjmatchedKind))
            [#else]
          else
            [/#if]

          {
          [#if lexerData.hasSkipActions]
                 tokenLexicalActions();
          [/#if]
          [#if multipleLexicalStates]
            if (newLexicalStates[jjmatchedKind] != null) {
               this.lexicalState = newLexicalStates[jjmatchedKind];
            }
          [/#if]

            continue EOFLoop;
          }
         [#if lexerData.hasMore]
          [#if lexerData.hasMoreActions]
          tokenLexicalActions();
          [#else]
          matchedCharsLength += jjmatchedPos + 1;
		  [/#if]
		  
          [#if multipleLexicalStates]
             doLexicalStateSwitch(jjmatchedKind);
          [/#if]
          curPos = 0;
          jjmatchedKind = 0x7FFFFFFF;
          int retval = input_stream.readChar();
          if (retval >=0) {
               curChar = retval;
	
	            [#var debugOutput]
	            [#set debugOutput]
	              [#if multipleLexicalStates]
	                 "<" + lexicalState + ">" + 
	              [/#if]
                  [#-- REVISIT --]
	              "Current character : " + addEscapes(String.valueOf(curChar)) + " (" + curChar + ") " +
	              "at line " + input_stream.getEndLine() + " column " + input_stream.getEndColumn()
	            [/#set]
	              if (trace_enabled) LOGGER.info(${debugOutput?trim});
	          continue;
	      }
     [/#if]
   [/#if]
   }
    return handleInvalidChar(curChar);
[#if lexerData.hasMore]
    }
[/#if]
     }
  }

  private InvalidToken handleInvalidChar(int ch) {
    int line = input_stream.getEndLine();
    int column = input_stream.getEndColumn();
    String img = new String(new int[] {ch}, 0, 1);
    if (invalidToken == null) {
       invalidToken = new InvalidToken(img, inputSource);
       invalidToken.setBeginLine(line);
       invalidToken.setBeginColumn(column);
    } else {
       invalidToken.setImage(invalidToken.getImage() + img);
    }
    invalidToken.setEndLine(line);
    invalidToken.setEndColumn(column);
    return invalidToken;
  }

  private void tokenLexicalActions() {
       switch(jjmatchedKind) {
   [#list lexerData.regularExpressions as regexp]
        [#if regexp.codeSnippet?has_content]
		  case ${regexp.ordinal} :
            [#if regexp.ordinal = 0]
              image.setLength(0); // For EOF no chars are matched
            [#else]
              image.append(input_stream.getSuffix(matchedCharsLength + jjmatchedPos + 1));
            [/#if]
		      ${regexp.codeSnippet.javaCode}
           break;
        [/#if]
   [/#list]
      }
    }

    private Token jjFillToken() {
        final String curTokenImage = input_stream.getImage();
        final int beginLine = input_stream.getBeginLine();
        final int beginColumn = input_stream.getBeginColumn();
        final int endLine = input_stream.getEndLine();
        final int endColumn = input_stream.getEndColumn();
    [#if grammar.settings.TOKEN_FACTORY??]
        final Token t = ${grammar.settings.TOKEN_FACTORY}.newToken(TokenType.values()[jjmatchedKind], curTokenImage, inputSource);
    [#elseif !grammar.hugeFileSupport]
        final Token t = Token.newToken(TokenType.values()[jjmatchedKind], curTokenImage, this);
    [#else]
        final Token t = Token.newToken(TokenType.values()[jjmatchedKind], curTokenImage, inputSource);
    [/#if]
        t.setBeginLine(beginLine);
        t.setEndLine(endLine);
        t.setBeginColumn(beginColumn);
        t.setEndColumn(endColumn);
//        t.setInputSource(this.inputSource);
     [#if false]
        t.setLexicalState(lexicalState);
     [/#if]        
        return t;
    }

[@OutputNfaStateMoves/]

[#list lexerData.lexicalStates as lexicalState]
   [@DumpDfaCode lexicalState/]
   [@DumpMoveNfa lexicalState/]
[/#list]

[#--
  NB. The following must occur after the preceding loop,
  since (and I don't like it) the DumpXXX macros
  build up the lexerData.orderedStateSets structure
  --]  
  private static final int[] jjnextStates = {
  [#list lexerData.orderedStateSets as set]
    [#list set as state]
        ${state.index},
    [/#list]
  [/#list]

};

[#macro OutputNfaStateMoves]  
    private final int[] jjstateSet = new int[${2*lexerData.stateSetSize}];
    private int jjnewStateCnt;
    private BitSet checkedStates = new BitSet();

    private void addState(int state) {
         if (!checkedStates.get(state)) {
            jjstateSet[jjnewStateCnt++] = state;
            checkedStates.set(state);
         }
    }
    
    private void addStates(int start, int count) {
        for (int i=0; i<count; i++) {
           addState(jjnextStates[start+i]);
        }
    }
   
    private int jjStopAtPos(int pos, int kind) {
         jjmatchedKind = kind;
         jjmatchedPos = pos;
         if (trace_enabled) LOGGER.info("   No more string literal token matches are possible.");
         if (trace_enabled) LOGGER.info("   Currently matched the first " + (jjmatchedPos + 1) 
                            + " characters as a " + tokenImage[jjmatchedKind] + " token.");
         return pos + 1;
    }
    [#var needNextStep = false]

   [#list lexerData.lexicalStates as lexicalState]
       [#list lexicalState.allStates as nfaState]
          [#if nfaState.moveRanges?size < 16]  
            private static final boolean ${nfaState.moveMethodName}(int ch) {
               [#var left, right]
               [#list nfaState.moveRanges as char]
                  [#if char_index % 2 = 0]
                     [#set left = char]
                  [#else]
                     [#set right = char]
                     [#if left = right]
                     if (ch == ${left}) return true;
                     [#else]
                       [#if left >0 ]
                     if (ch < ${left}) return false;
                       [/#if]
                     if (ch <= ${right}) return true;
                     [/#if]
                  [/#if]
               [/#list]
                     return false;
            }
           [#else]
            [#set needNextStep = true]
            [#var arrayName = nfaState.movesArrayName]

            static private int[] ${arrayName};

            static private void ${arrayName}_populate() {
               ${arrayName} = new int[${nfaState.moveRanges?size}];
               [#list nfaState.moveRanges as char]
                  ${arrayName}[${char_index}] = ${char};
               [/#list]
            }

            private static final boolean ${nfaState.moveMethodName}(int ch) {
               int idx = Arrays.binarySearch(${arrayName}, ch);
               return idx>=0 || idx%2==0;
            }
           [/#if]
       [/#list]
   [/#list]
     [#if needNextStep]
     static {
       [#list lexerData.lexicalStates as lexicalState]
        [#list lexicalState.allStates as nfaState]
          [#if nfaState.moveRanges?size >= 16]
            ${nfaState.movesArrayName}_populate();
          [/#if]
         [/#list]
       [/#list]
     }
     [/#if]
[/#macro]

[#macro DumpMoveNfa lexicalState]
  [#var hasNfa = lexicalState.numNfaStates>0]
    private int jjMoveNfa_${lexicalState.name}(int startState, int curPos) {
    [#if !hasNfa]
        return curPos;
    }
       [#return]
    [/#if]
        int strKind = jjmatchedKind;
        int strPos = jjmatchedPos;
        int seenUpto = curPos+1;
        input_stream.backup(seenUpto);
        curChar = input_stream.readChar(); //REVISIT, deal with error return code
        curPos = 0;
        int startsAt = 0;
        jjnewStateCnt = ${lexicalState.numNfaStates};
        int stateIndex=1;
        jjstateSet[0] = startState;
        int kind = 0x7fffffff;
        while (true) {
            checkedStates.clear();
	         do {
	             switch (jjstateSet[--stateIndex]) {
	                 [@DumpMoves lexicalState/]
                     default : break;
                }
            } while(stateIndex != startsAt);
            if (kind != 0x7fffffff) {
                jjmatchedKind = kind;
                jjmatchedPos = curPos;
                kind = 0x7fffffff;
            }
            ++curPos;
            if (jjmatchedKind != 0 && jjmatchedKind != 0x7fffffff) {
                if (trace_enabled) LOGGER.info("   Currently matched the first " + (jjmatchedPos +1) + " characters as a " 
                                     + tokenImage[jjmatchedKind] + " token.");
            }
            stateIndex = jjnewStateCnt;
            jjnewStateCnt = startsAt;
            startsAt = ${lexicalState.numNfaStates} - startsAt;
            if (stateIndex == startsAt)
                 break;
            int retval = input_stream.readChar();
            if (retval >=0) {
                 curChar = retval;
            }
            else  {
                break;
            }
            if (trace_enabled) LOGGER.info("" + 
            [#if multipleLexicalStates]
               "<" + lexicalState + ">" + 
            [/#if]
               [#-- REVISIT --]
               addEscapes(String.valueOf(curChar)) + " (" + curChar + ") "
              + "at line " + input_stream.getEndLine() + " column " + input_stream.getEndColumn());
        }
        if (jjmatchedPos > strPos) {
            return curPos;
        }
        int toRet = Math.max(curPos, seenUpto);
        if (curPos < toRet) {
           for (int i = toRet - Math.min(curPos, seenUpto); i-- >0;) {
                   curChar = input_stream.readChar(); // REVISIT, not handling error return code
           }
        }
        if (jjmatchedPos < strPos) {
            jjmatchedKind = strKind;
            jjmatchedPos = strPos;
        }
        else if (jjmatchedPos == strPos && jjmatchedKind > strKind) {
            jjmatchedKind = strKind;
        }
        return toRet;
    }
[/#macro]

[#macro DumpMoves lexicalState]
   [#list lexicalState.allCompositeStateSets as stateSet]
       [#var stateIndex=lexicalState.getStartStateIndex(stateSet)]
       case ${stateIndex} :
        [#list stateSet as state]
             [@DumpMoveForCompositeState state/]
        [/#list]
          break;
   [/#list]
   [#list lexicalState.allStates as state]
       case ${state.index} :
         [@DumpMove state /]
   [/#list]
[/#macro]

[#macro DumpMoveForCompositeState nfaState]
   [#var nextState = nfaState.nextState]
   [#var lexicalState=nfaState.lexicalState]
   [#var kindToPrint=(nextState.type.ordinal)!MAX_INT]
      if (${nfaState.moveMethodName}(curChar)) {
   [#if kindToPrint != MAX_INT]
      kind = Math.min(kind, ${kindToPrint});
   [/#if]
   [#if !nextState?is_null&&nextState.epsilonMoveCount>0]
       [#-- Note that the getStartIndex() method builds up a needed
            data structure lexicalState.orderedStateSet, which is used to output
            the jjnextStates vector. --]
       [#var index = lexicalState.getStartIndex(nextState)]
       addStates(${index}, ${nextState.epsilonMoveCount});
   [/#if]
         }
[/#macro]

[#macro DumpMove nfaState]
   [#var nextState = nfaState.nextState]
   [#var lexicalState=nfaState.lexicalState]
   [#var kindToPrint=(nextState.type.ordinal)!MAX_INT]
   [#if nextState?is_null || nextState.epsilonMoveCount==0]
         [#var kindCheck=" && kind > "+kindToPrint]
         [#if kindToPrint == MAX_INT][#set kindCheck = ""][/#if]
            if (${nfaState.moveMethodName}(curChar) ${kindCheck})
               kind = ${kindToPrint};
            break;
         [#return]
   [/#if]
   [#if kindToPrint != MAX_INT]
                if (!${nfaState.moveMethodName}(curChar))
                          break;
                    kind = Math.min(kind, ${kindToPrint});
   [#else]
                    if (${nfaState.moveMethodName}(curChar))
   [/#if]
   [#if !nextState?is_null&&nextState.epsilonMoveCount>0]
       [#var index = lexicalState.getStartIndex(nextState)]
       addStates(${index}, ${nextState.epsilonMoveCount});
   [/#if]
       break;
[/#macro]

[#macro DumpDfaCode lexicalState]
  [#var initState=lexicalState.initialStateIndex]
  [#var maxStringLength=lexicalState.maxStringLength]
  [#var maxStringIndex=lexicalState.maxStringIndex]
  [#var maxStringLengthForActive=lexicalState.maxStringLengthForActive]
  [#var hasNfa = lexicalState.numNfaStates>0]
  [#if maxStringLength = 0]
    private int jjMoveStringLiteralDfa0_${lexicalState.name}() {
    [#if hasNfa]
        return jjMoveNfa_${lexicalState.name}(${initState}, 0);
    [#else]
        return 1;        
    [/#if]
    }
    [#return]
  [/#if]
  
  [#list lexicalState.stringLiteralTables as table]
    [#var startNfaNeeded=false]
    [#var first = (table_index==0)]
    
    private int jjMoveStringLiteralDfa${table_index}_${lexicalState.name}
    [@ArgsList]
        [#list 0..maxStringIndex/64 as j]
           [#if !first && table_index<=maxStringLengthForActive[j]+1&&maxStringLengthForActive[j] != 0]
              [#if table_index != 1]
                 long old${j}
              [/#if]
               long active${j}
           [/#if]
        [/#list]
    [/@ArgsList] {
    [#if !first]
      [#if table_index > 1]
         [#list 0..maxStringIndex/64 as j]
           [#if table_index<=lexicalState.maxStringLengthForActive[j]+1]
        active${j} = active${j} & old${j};
           [/#if]
         [/#list]
        if ([@ArgsList delimiter=" | "]
         [#list 0..maxStringIndex/64 as j]
           [#if table_index<=lexicalState.maxStringLengthForActive[j]+1]
            active${j}
           [/#if]
         [/#list]
         [/@ArgsList] == 0L)
         [#if hasNfa]
            return jjMoveNfa_${lexicalState.name}(${initState}, ${table_index-1});
         [#else]
            return ${table_index};
         [/#if]   
      [/#if]
       int retval = input_stream.readChar();
       if (retval >=0) {
           curChar = retval;
       }
       else  {
         [#if hasNfa]
           return jjMoveNfa_${lexicalState.name}(${initState}, ${table_index-1}); 
         [#else]
           return ${table_index};
         [/#if]
       }
    [/#if]
    [#if !first]
      if (trace_enabled) LOGGER.info("" + 
        [#if lexerData.lexicalStates?size != 1]
           "<${lexicalState.name}>" +
        [/#if]
        [#-- REVISIT --]
        "Current character : " + addEscapes(String.valueOf(curChar)) + " ("
        + curChar + ") at line " + input_stream.getEndLine() + " column " + input_stream.getEndColumn());
    [/#if]
      switch (curChar) {
    [#list table?keys as key]
       [#var info = lexicalState.getKindInfo(table, key)]
	    [#var c = utils.codePointAsString(key)]
       [#var ifGenerated=false]
	   [#if lexicalState.generateDfaCase(key, info, table_index)]
	      [#-- We know key is a single character.... --]
	      [#if grammar.ignoreCase][#--REVISIT--]
	         [#if c != c?upper_case]
	           case ${utils.firstCharAsInt(c?upper_case)} :
	         [/#if]
	         [#if c != c?lower_case]
	           case ${utils.firstCharAsInt(c?lower_case)} : 
	         [/#if]
	      [/#if]
	           case ${utils.firstCharAsInt(c)} :
	      [#if info.finalKindCnt != 0]
	        [#list 0..maxStringIndex as kind]
	          [#var matchedKind=info.finalKinds[(kind/64)?int]]
              [#if utils.isBitSet(matchedKind, kind%64)]
                 [#if ifGenerated]
                 else if 
                 [#elseif table_index != 0]
                 if 
                 [/#if]
                 [#set ifGenerated = true]
                 [#if table_index != 0]
                   ((active${(kind/64)?int} & ${utils.powerOfTwoInHex(kind%64)}) != 0L) 
                 [/#if]
                 [#if table_index != 0]
                    {
                    jjmatchedKind = ${kind};
                    jjmatchedPos = ${table_index};
                    }
                    [#else]
                    jjmatchedKind = ${kind};
                 [/#if]
              [/#if]
	        [/#list]
	      [/#if]
	      [#if info.validKindCnt != 0]
	           return jjMoveStringLiteralDfa${table_index+1}_${lexicalState.name}[@ArgsList]
	              [#list 0..maxStringIndex/64 as j]
	                 [#if table_index<=maxStringLengthForActive[j]&&maxStringLengthForActive[j] != 0]
	                    [#if table_index != 0]
	                       active${j}
	                    [/#if]
	                    ${utils.toHexStringL(info.validKinds[j])}
	                 [/#if]
	              [/#list]
	           [/@ArgsList];
	      [#else][#-- a very special case--]
	        [#if table_index = 0]
	           [#if hasNfa]
	           return jjMoveNfa_${lexicalState.name}(${initState}, 0);
	           [#else]
	           return 1;
	           [/#if]
	        [#elseif table_index != 0][#-- No more str literals to look for --]
	           break;
	           [#set startNfaNeeded = true]
	        [/#if]
	      [/#if]
	   [/#if]       
    [/#list]
    [#-- default means that the current characters is not in any of
    the strings at this position--]
         default : 
            if (trace_enabled) LOGGER.info("   No string literal matches possible.");
    [#if hasNfa]
       [#if table_index = 0]
            return jjMoveNfa_${lexicalState.name}(${initState}, 0);
       [#else]
            break;
          [#set startNfaNeeded = true]
       [/#if]
    [#else]
           return ${table_index+1};
    [/#if]
      }
    [#if table_index != 0]
       [#if startNfaNeeded]
          [#if hasNfa]
             return jjMoveNfa_${lexicalState.name}(${initState}, ${table_index});
          [#else]
             return ${table_index+1};
          [/#if]        
       [/#if]
    [/#if]
   }
  [/#list]
[/#macro] 

[#macro SkipSingles lexicalState]
    [#if lexicalState.hasSinglesToSkip]
       [#var byteMask1 = utils.toHexStringL(lexicalState.getSinglesToSkip(0))]
       [#var byteMask2 = utils.toHexStringL(lexicalState.getSinglesToSkip(1))]
       while ((curChar < 64 && ((${byteMask1} & (1L << curChar)) != 0)) 
             || (curChar >=64 && curChar < 128 && (${byteMask2} & (1L<<(curChar-64)))!=0))
            {
               [#var debugOutput]
               [#set debugOutput]
               [#if lexerData.lexicalStates?size > 1]
               "<" + lexicalState + ">" + 
               [/#if]
               [#-- REVISIT --]
               "Skipping character : " + addEscapes(String.valueOf(curChar)) + " (" + curChar + ")"
               [/#set] 
               if (trace_enabled) LOGGER.info(${debugOutput?trim}); 
               curChar = input_stream.beginToken();
               if (curChar == -1) {
                  return generateEOF();
               }
            }
   [/#if]
[/#macro]

[#---
   Utility macro to output a sequence of args, typically
   to a method. The input can be passed in as an argument,
   or via the macro's nested content. In either case, it
   is just one argument per line. The macro takes care of 
   commas and the opening and closing parentheses.  
--]   

[#macro ArgsList input="" delimiter=","]
   [#if input?length = 0]
     [#set input]
       [#nested]
     [/#set]
   [/#if]
   [#set input = input?trim?split("
")]
   (
   [#list input as arg]
      [#set arg = arg?trim]
      [#if arg?length != 0]
        ${arg}
        [#if arg_has_next]
           ${delimiter} 
        [/#if]
      [/#if]
   [/#list] 
   )
[/#macro]
