/* Copyright (c) 2008-2021 Jonathan Revusky, revusky@javacc.com
 * Copyright (c) 2006, Sun Microsystems Inc.
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
 *       nor the names of any contributors may be used to endorse or promote
 *       products derived from this software without specific prior written
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
package com.javacc.lexgen;

import java.util.*;

import com.javacc.Grammar;
import com.javacc.parsegen.RegularExpression;
import com.javacc.parser.Token;
import com.javacc.parser.tree.CodeBlock;
import com.javacc.parser.tree.RegexpChoice;
import com.javacc.parser.tree.RegexpSpec;
import com.javacc.parser.tree.RegexpStringLiteral;
import com.javacc.parser.tree.TokenProduction;
import static java.lang.Math.min; 

public class LexicalStateData {

    private Grammar grammar;
    private LexerData lexerData;
    private String name;
    private NfaState initialState;
    private BitSet singlesToSkipSet = new BitSet();
    private BitSet subStringSet = new BitSet();
    private BitSet subStringAtPosSet = new BitSet();
 

    Map<Integer, NfaState> indexedAllStates = new HashMap<>();
    Set<NfaState> allStates = new HashSet<>();
    private Set<Set<NfaState>> allCompositeStateSets = new HashSet<>();
    private int dummyStateIndex = -1;
    private Map<Set<NfaState>, Integer> stateIndexFromStateSet = new HashMap<>();
    private List<TokenProduction> tokenProductions = new ArrayList<>();
    private Map<String, RegularExpression> caseSensitiveTokenTable = new HashMap<>();
    private Map<String, RegularExpression> caseInsensitiveTokenTable = new HashMap<>();

    private HashSet<RegularExpression> regularExpressions = new HashSet<>();

    // This is a list where the index corresponds to the offset
    // into the string literal and the items are maps of integers,
    // codepoints actually, to KindInfo objects. The KindInfo 
    // objects represent the set of tokens that can still be matched by
    // that character (i.e. codepoint) and also the ones that can be
    // terminated. (Accepting state in the Aho et al. terminology)
    private List<Map<Integer, KindInfo>> stringLiteralTables = new ArrayList<>();

    LexicalStateData(Grammar grammar, String name) {
        this.grammar = grammar;
        this.lexerData = grammar.getLexerData();
        this.name = name;
        this.initialState = new NfaState(this);
    }

    Grammar getGrammar() {
        return grammar;
    }
   
    NfaState getInitialState() {return initialState;}

    public String getName() {return name;}

    public long getSinglesToSkip(int byteNum) {
        long[] ll = singlesToSkipSet.toLongArray();
        return ll.length > byteNum ? ll[byteNum] : 0L;
    }

    public boolean getHasSinglesToSkip() {
        return singlesToSkipSet.cardinality()>0;
    }

    public int getInitialStateIndex() {
        return getStartStateIndex(initialState.epsilonMoves);
    }

    public Collection<NfaState> getAllStates() {
        return indexedAllStates.values();
    }

    public Set<Set<NfaState>> getAllCompositeStateSets() {
        return allCompositeStateSets;
    }

    public int getMaxStringLength() {
        int result = 0;
        for (RegularExpression re : regularExpressions) {
            int length = re.getImage() == null ? 0 : re.getImage().length();
            result = Math.max(result, length);
        }
        return result;
    }
    
    public int getMaxStringIndex() {
        int result =0;
        for (RegularExpression re: regularExpressions) {
            if (re instanceof RegexpStringLiteral  && re.getImage().length()>0) 
                result = Math.max(result,re.getOrdinal()+1);
        }
        return result;
    }

    public int getMaxStringLengthForActive(int byteNum){
        int result = 0;
        int leftBound = byteNum*64;
        int rightBound = min(grammar.getLexerData().getTokenCount(), leftBound+64);
        for (int i = leftBound; i< rightBound; i++) {
            String image = grammar.getLexerData().getRegularExpression(i).getImage();
            if (image !=null && image.length() > result) {
                result = image.length();
            }
        }
        return result;
    }

    void addTokenProduction(TokenProduction tokenProduction) {
        tokenProductions.add(tokenProduction);
    }

    public int getNumNfaStates() {
        return indexedAllStates.size();
    }

    public boolean containsRegularExpression(RegularExpression re) {
        return regularExpressions.contains(re);
    }

    public void addStringLiteral(RegexpStringLiteral re) {
        if (re.getIgnoreCase()) {
            caseInsensitiveTokenTable.put(re.getImage().toUpperCase(), re);
        } else {
            caseSensitiveTokenTable.put(re.getImage(), re);
        }
    }

    public RegularExpression getStringLiteral(String image) {
        RegularExpression result = caseSensitiveTokenTable.get(image);
        if (result == null) {
            result = caseInsensitiveTokenTable.get(image.toUpperCase());
        }
        return result;
    }

    List<RegexpChoice> process() {
    	List<RegexpChoice> choices = new ArrayList<>();
        boolean isFirst = true;
        for (TokenProduction tp : tokenProductions) {
            choices.addAll(processTokenProduction(tp, isFirst));
            isFirst = false;
        }
        generateDfaData();
        generateNfaData();
        return choices;
    }

    private void generateDfaData() {
        fillSubString();
        for (int i = 0; i < getMaxStringLength(); i++) {
            Map<Integer, KindInfo> table = getStringLiteralTables().get(i);
            for (Integer key : table.keySet()) {
                generateDfaCase(key, table.get(key), i);
            }
        }
    }

    private List<RegexpChoice> processTokenProduction(TokenProduction tp, boolean isFirst) {
        boolean ignore = tp.isIgnoreCase() || grammar.isIgnoreCase();//REVISIT
        List<RegexpChoice> choices = new ArrayList<>();
        for (RegexpSpec respec : tp.getRegexpSpecs()) {
            RegularExpression currentRegexp = respec.getRegexp();
            regularExpressions.add(currentRegexp);
//            currentRegexp.setIgnoreCase(ignore);
            if (currentRegexp.isPrivate()) {
                continue;
            }
            if (currentRegexp instanceof RegexpStringLiteral
                    && !((RegexpStringLiteral) currentRegexp).getImage().equals("")) {
                generate((RegexpStringLiteral) currentRegexp);
            } else {
                if (currentRegexp instanceof RegexpChoice) {
                    choices.add((RegexpChoice) currentRegexp);
                }
                new NfaBuilder(this, ignore).buildStates(currentRegexp);
            }
            if (respec.getNextState() != null && !respec.getNextState().equals(this.name))
                currentRegexp.setNewLexicalState(lexerData.getLexicalState(respec.getNextState()));

            if (respec.getCodeSnippet() != null && !respec.getCodeSnippet().isEmpty()) {
                currentRegexp.setCodeSnippet(respec.getCodeSnippet());
            }
            CodeBlock tokenAction = currentRegexp.getCodeSnippet();
            String kind = tp.getKind();
            if (kind.equals("SPECIAL_TOKEN")) {
                if (tokenAction != null || currentRegexp.getNewLexicalState() != null) {
                    lexerData.hasSkipActions = true;
                }
                lexerData.hasSpecial = true;
                if (currentRegexp.getOrdinal() >0) {
                    lexerData.getSpecialSet().set(currentRegexp.getOrdinal());
                    lexerData.getSkipSet().set(currentRegexp.getOrdinal());
                }
                currentRegexp.setUnparsedToken();
            }
            else if (kind.equals("SKIP")) {
                lexerData.hasSkipActions |= (tokenAction != null);
                lexerData.hasSkip = true;
                lexerData.getSkipSet().set(currentRegexp.getOrdinal());
                currentRegexp.setSkip();
            }
            else if (kind.equals("MORE") && currentRegexp.getOrdinal()>0) { // REVISIT
                lexerData.hasMoreActions |= tokenAction != null;
                lexerData.hasMore = true;
                lexerData.getMoreSet().set(currentRegexp.getOrdinal());
                currentRegexp.setMore();
            }
            else if (currentRegexp.getOrdinal() >0) { // REVISIT
                lexerData.getTokenSet().set(currentRegexp.getOrdinal());
                currentRegexp.setRegularToken();
            }
        }
        return choices;
    }


    private void generateNfaData() {
        for (NfaState state : allStates) {
            state.doEpsilonClosure();
        }
        initialState.generateCode();
        int initialOrdinal = initialState.getType() == null ? -1 : initialState.getType().getOrdinal();
        if (initialState.getType() != null && initialOrdinal != 0) {
            if (lexerData.getSkipSet().get(initialOrdinal)
                || (lexerData.getSpecialSet().get(initialOrdinal)))
                lexerData.hasSkipActions = true;
            else if (lexerData.getMoreSet().get(initialOrdinal))
                lexerData.hasMoreActions = true;
        }
        allStates.removeIf(state->state.getIndex()==-1);
    }

    private boolean canStartNfaUsing(int c) {
        return initialState.epsilonMoves.stream().anyMatch(state->state.canMoveUsingChar(c));
    }

    public int getStartStateIndex(String stateSetString) {
        return getStartStateIndex(stateSetFromString(stateSetString));
    }

    public int getStartStateIndex(Set<NfaState> states) {
        if (states.isEmpty()) return -1;
        if (stateIndexFromStateSet.containsKey(states)) {
            return stateIndexFromStateSet.get(states);
        }
        List<Integer> nameSet = new ArrayList<>();
        for (NfaState state : states) nameSet.add(state.getIndex());
        if (nameSet.size() == 1) {
            stateIndexFromStateSet.put(states, nameSet.get(0));
            return nameSet.get(0);
        }
        if (dummyStateIndex == -1) {
            dummyStateIndex = indexedAllStates.size();
        } else {
            ++dummyStateIndex;
        }
        stateIndexFromStateSet.put(states, dummyStateIndex);
        allCompositeStateSets.add(states);
        return dummyStateIndex;
    }

    private Set<NfaState> stateSetFromString(String stateSetString) {
        Set<NfaState> result = new HashSet<>();
        List<Integer> indexes = epsilonMovesStringToIntArray(stateSetString);
        for (int index : indexes) {
            NfaState state = indexedAllStates.get(index);
            result.add(state);
        }
        return result;
    }    

    static private List<Integer> epsilonMovesStringToIntArray(String s) {
        List<Integer> result = new ArrayList<>();
        StringTokenizer st = new StringTokenizer(s, "{},;null", false);
        while (st.hasMoreTokens()) {
            result.add(Integer.valueOf(st.nextToken()));
        }
        return result;
    }

    public int getStartIndex(NfaState state) {
        Integer result = lexerData.getTableToDump().get(state.epsilonMoves);
        if (result == null) {
            result = lexerData.lastIndex;
            lexerData.lastIndex += state.getEpsilonMoveCount();
            lexerData.getTableToDump().put(state.epsilonMoves, result);
            lexerData.getOrderedStateSets().add(state.epsilonMoves);
        }
        return result;
    }

    private void fillSubString() {
        int maxStringIndex = getMaxStringIndex();
        for (int i = 0; i < maxStringIndex; i++) {
            RegularExpression re = grammar.getLexerData().getRegularExpression(i);
            subStringSet.clear(i);
            if (re.getImage() == null || !containsRegularExpression(re)) {
                continue;
            }
            subStringSet.set(i);
            subStringAtPosSet.set(re.getImage().length() - 1);
        }
    }

    public List<Map<Integer, KindInfo>> getStringLiteralTables() {
        return stringLiteralTables;
    }

    public boolean generateDfaCase(int ch, KindInfo info, int index) {
        int maxStringIndex = getMaxStringIndex();
        for (int kind = 0; kind < maxStringIndex; kind++) {
        	if (index == 0 && ch < 128 && info.getFinalKindCnt() !=0
        			&& (getNumNfaStates()==0 || !canStartNfaUsing(ch))) {
        			if (info.isFinalKind(kind) && !subStringSet.get(kind)) {
                        if (grammar.getLexerData().getSkipSet().get(kind)
        				        && !grammar.getLexerData().getSpecialSet().get(kind)
        						&& grammar.getLexerData().getRegularExpression(kind).getCodeSnippet() == null
        						&& grammar.getLexerData().getRegularExpression(kind).getNewLexicalState() == null) {
                            singlesToSkipSet.set(ch);
                            //REVISIT
        					if (grammar.isIgnoreCase()) {
                                singlesToSkipSet.set(Character.toUpperCase(ch));
                                singlesToSkipSet.set(Character.toLowerCase(ch));
        					}
        					return false;
        				}
        			}
        	}
        }
        return true;
    }

    private void generate(final RegexpStringLiteral rsLiteral) {
        final int ordinal = rsLiteral.getOrdinal();
        final String stringLiteral = rsLiteral.getImage();
        final int stringLength = stringLiteral.length();
        while (stringLiteralTables.size() < stringLength) {
            stringLiteralTables.add(new HashMap<>());
        }
        for (int i = 0; i < stringLength; i++) {
            int c = stringLiteral.codePointAt(i);
            if (c > 0xFFFF) i++;
            if (grammar.isIgnoreCase()) {
               c = Character.toLowerCase(c);
            }
            Map<Integer, KindInfo> table = stringLiteralTables.get(i);
            if (!table.containsKey(c)) {
                table.put(c, new KindInfo(grammar));
            }
            KindInfo info = table.get(c);
            if (!grammar.isIgnoreCase() && rsLiteral.getIgnoreCase()) {
                table.put(Character.toLowerCase(c), info);
                table.put(Character.toLowerCase(c), info);
            }
            if (i + 1 == stringLength) {
                info.insertFinalKind(ordinal);
            }
            else {
                info.insertValidKind(ordinal);
            }
        }
    }

    // This method is just a temporary kludge
    // prior to a more complete refactoring.
    // It is only called from the DfaCode.java.ftl template.
    public KindInfo getKindInfo(Map<Integer, KindInfo> table, int key) {
        return table.get(key);
    }
}