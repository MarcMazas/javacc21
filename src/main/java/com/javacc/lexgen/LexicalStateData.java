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
import com.javacc.parser.tree.CodeBlock;
import com.javacc.parser.tree.RegexpChoice;
import com.javacc.parser.tree.RegexpSpec;
import com.javacc.parser.tree.RegexpStringLiteral;
import com.javacc.parser.tree.TokenProduction;

public class LexicalStateData {

    private Grammar grammar;
    private LexerData lexerData;
    private String name;
    private DfaData dfaData;
    private NfaState initialState;  

    Map<Integer, NfaState> indexedAllStates = new HashMap<>();
    Set<NfaState> allStates = new HashSet<>();
    private Set<Set<NfaState>> allCompositeStateSets = new HashSet<>();
    private int dummyStateIndex = -1;
    private Map<Set<NfaState>, Integer> stateIndexFromStateSet = new HashMap<>();
    private List<TokenProduction> tokenProductions = new ArrayList<>();
    private Map<String, RegularExpression> caseSensitiveTokenTable = new HashMap<>();
    private Map<String, RegularExpression> caseInsensitiveTokenTable = new HashMap<>();

    private HashSet<RegularExpression> regularExpressions = new HashSet<>();

    public LexicalStateData(Grammar grammar, String name) {
        this.grammar = grammar;
        this.lexerData = grammar.getLexerData();
        this.dfaData = new DfaData(this);
        this.name = name;
        this.initialState = new NfaState(this);
    }

    Grammar getGrammar() {
        return grammar;
    }
   
    NfaState getInitialState() {return initialState;}

    public String getName() {return name;}

    public DfaData getDfaData() {return dfaData;}

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
        dfaData.generateData();
        generateData();
        return choices;
    }

    List<RegexpChoice> processTokenProduction(TokenProduction tp, boolean isFirst) {
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
                dfaData.generate((RegexpStringLiteral) currentRegexp);
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


    void generateData() {
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

    boolean canStartNfaUsing(int c) {
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
}
