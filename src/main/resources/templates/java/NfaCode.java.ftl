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

 [--
   This file contains the macro that generates the Java code 
   for the NFA. Needless to say, this still needs some cleanup.
 --]

[#var utils = grammar.utils, lexerData=grammar.lexerData]
[#var multipleLexicalStates = lexerData.lexicalStates?size >1]
[#var MAX_INT=2147483647]

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