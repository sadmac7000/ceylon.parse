import ceylon.language.meta {
    _type = type
}
import ceylon.language.meta.model {
    Type,
    Generic,
    UnionType
}
import ceylon.collection {
    HashSet,
    HashMap,
    ArrayList,
    PriorityQueue
}

"A single token result returned by a tokenizer"
shared class Token<out SymType = Object>(SymType sym, Integer length)
        extends Symbol(typeAtomCache.getAlias(`SymType`), sym, length)
        given SymType satisfies Object {}

"A parsed symbol."
shared class Symbol(shared Integer type, shared Object sym, shared Integer length) {
    shared actual Integer hash => type ^ 2 + length;

    shared actual Boolean equals(Object that) {
        if (! is Symbol that) {
            return false;
        } else {
            if (_type(this) != _type(that)) { return false; }

            return this.length == that.length;
        }
    }
}

"A result to represent the end of a stream."
shared Token eos = Token(eosObject, 0);
object eosObject {}

"We have to convert type objects to integers to pass them around, otherwise we
 encounter weird performance issues."
object typeAtomCache {
    value from = HashMap<Type, Integer>();
    value to = HashMap<Integer, Type>();
    variable value next = 0;

    "Get an alias for a type"
    shared Integer getAlias(Type t) {
        if (from.defines(t)) {
            value ret = from[t];
            assert(exists ret);
            return ret;
        }

        from.put(t, next);
        to.put(next, t);
        return next++;
    }

    "Resolve a type"
    shared Type resolve(Integer i) {
        value ret = to[i];
        assert(exists ret);
        return ret;
    }
}

"A rule. Specifies produced and consumed symbols and a method to execute them"
shared class Rule(shared Object(Object*) consume, shared Integer[] consumes,
        shared Integer produces) {
    shared actual Integer hash = consumes.hash ^ 2 + produces.hash;

    shared actual Boolean equals(Object other) {
        if (is Rule other) {
            return other.consumes == consumes && other.produces == produces;
        } else {
            return false;
        }
    }
}

"Exception thrown when we need an error constructor but one isn't defined"
class ErrorConstructorException(Type token)
        extends Exception("Could not construct error of type ``token``") {}

"Exception thrown when we need a bad token constructor but one isn't defined"
class BadTokenConstructorException()
        extends Exception("Could not construct invalid token") {}

"An error in parsing"
abstract class Error() {
    shared actual Boolean equals(Object that) {
        return _type(this) == _type(that);
    }
}

"Error resolved by replacing a token"
class ErrorReplace(shared Object(Object?) construct,
        shared Object original) extends Error() {}

"Error resolved by deleting a token"
class ErrorDelete(shared Object original) extends Error() {}

"Error resolved by inserting a new token"
class ErrorInsert(shared Object(Object?) construct) extends Error() {}

"An Earley parser state"
class EPState(pos, rule, matchPos, start, children, baseLsd,
        errorConstructors, tokensProcessedBefore, overrideLastToken = null) {
    "Starting Levenshtein distance"
    Integer baseLsd;

    "Token we processed before parsing this rule"
    Object? overrideLastToken;

    "Tokens processed before we began to parse this state"
    Integer tokensProcessedBefore;

    "Starting position for the rule match"
    shared Integer start;

    "Error constructors"
    shared Map<Integer,Object(Object?)> errorConstructors;

    "Tokens"
    shared [Symbol|EPState|Error*] children;

    "The last token processed before matchPos"
    shared Object? lastToken {
        if (exists overrideLastToken) {
            return null;
        }
        value last = children.last;
        if (is Symbol last) {
            return last.sym;
        } else if (is EPState last) {
            return last.lastToken;
        } else /* null OR an error token */ {
            /* We're either at the start, or overrideLastToken should have been
             * set.
             */
            return null;
        }
    }

    assert(matchPos <= children.size);

    "Number of tokens matched. It is important that this does not count error
     tokens."
    shared Integer tokensProcessed = sum({ for (c in children) if (is EPState c)
        c.tokensProcessed }.chain({ for (c in children) if (is Symbol c) 1
        }).chain({0})) + tokensProcessedBefore;

    "Position this state belongs to"
    shared Integer pos;

    "Position within the rule we are matching"
    shared Integer matchPos;

    "The rule we are matching"
    shared Rule rule;

    variable Integer? _lsd = null;

    "Levenshtein distance between what we matched after error correction and
     the real string"
    shared Integer lsd {
        if (exists l=_lsd) { return l; }

        variable value ret = baseLsd;

        for (c in children) {
            if (is EPState c) {
                ret += c.lsd;
            }
        }

        _lsd = ret;
        return ret;
    }

    shared actual Integer hash = start ^ 4 +
        pos ^ 3 + matchPos ^ 2 + rule.hash;

    "Whether this state is complete"
    shared Boolean complete = rule.consumes.size == matchPos;

    "Whether this state has propagated from its position"
    variable Boolean propagated = complete;

    "The AST node for this state"
    shared Symbol astNode {
        assert(complete);

        variable Object[] sym = [];

        for (c in children) {
            if (is Symbol c) {
                sym = sym.withTrailing(c.sym);
            } else if (is EPState c) {
                sym = sym.withTrailing(c.astNode.sym);
            } else if (is ErrorInsert c) {
                sym = sym.withTrailing(c.construct(null));
            } else if (is ErrorDelete c) {
                /* Notify deletion */
            } else if (is ErrorReplace c) {
                sym = sym.withTrailing(c.construct(c.original));
            }
        }

        value s = Symbol(rule.produces, rule.consume(*sym), pos - start);
        return s;
    }

    "Derive a new state"
    EPState derive(Integer newPos=pos, Integer newMatchPos=matchPos,
            Symbol|EPState|Error?  newChild=null, Boolean error=false,
            Object? overrideLast = null) {
        Integer newBaseLsd;
        [Symbol|EPState|Error*] newChildren;

        if (error) {
            newBaseLsd = baseLsd + 1;
        } else {
            newBaseLsd = baseLsd;
        }

        if (exists newChild) {
            newChildren = children.withTrailing(newChild);
        } else {
            newChildren = children;
        }

        return EPState(newPos, rule, newMatchPos, start, newChildren,
                newBaseLsd, errorConstructors, tokensProcessedBefore,
                overrideLast);
    }

    "Propagate this state with a trailing error."
    shared {EPState *} failPropagate({Token *} skip,
            Boolean badToken) {
        assert(exists next = rule.consumes[matchPos]);
        value inscons = errorConstructors[next];

        if (! inscons exists) {
            throw ErrorConstructorException(typeAtomCache.resolve(next));
        }

        assert(exists inscons);

        value delete = { for (s in skip) derive(pos + s.length,
                matchPos, ErrorDelete(s.sym), true, s.sym)
        };

        value replace = { for (s in skip) derive(pos + s.length,
                matchPos + 1, ErrorReplace(inscons, s.sym), true,
                lastToken)
        };

        if (badToken) {
            return delete.chain(replace);
        }

        value insert = derive(pos, matchPos + 1, ErrorInsert(inscons), true,
                lastToken);

        return delete.chain(replace).chain({insert});
    }

    "Offer a symbol to this state for scanning or completion"
    shared EPState? feed(Symbol|EPState other) {
        assert(exists want = rule.consumes[matchPos]);

        if (is Symbol other) {
            if (want != other.type) { return null; }

            return derive(pos + other.length, matchPos + 1, other);
        } else {
            if (want != other.rule.produces) { return null; }

            return derive(other.pos, matchPos + 1, other);
        }
    }

    "Generate a prediction set for this state"
    shared {EPState *} propagate({Rule *} rules, Symbol? nextToken) {
        if (propagated) {
            return {};
        }

        propagated = true;

        {EPState *} predict = {
            for (other in rules)
                if (exists c=rule.consumes[matchPos], other.produces == c)
                    EPState(pos, other, 0, pos, [], 0, errorConstructors,
                            tokensProcessed, lastToken)
        };

        if (exists nextToken,
            exists x = feed(nextToken)) {
            return predict.chain({x});
        }

        return predict;
    }

    shared actual Boolean equals(Object other) {
        if (is EPState other) {
            if (other.start != start) { return false; }
            if (other.pos != pos) { return false; }
            if (other.rule != rule) { return false; }
            if (other.matchPos != matchPos) { return false; }

            if (other.children.size != children.size) { return false;  }

            for (a -> b in zipEntries(other.children, children)) {
                if (a != b) { return false; }
            }

            return true;
        } else {
            return false;
        }
    }

    "Checks which of two states (this and another) would make the best recovery
     token. The least state, by the returned comparison, is the winner"
    shared Comparison compareRecovery(EPState other, {Rule *} rules) {
        HashSet<Integer> productions = HashSet<Integer>{elements={for (r in
                rules) r.produces};};

        assert(exists otherNext = other.rule.consumes[other.matchPos]);
        assert(exists next = rule.consumes[matchPos]);
        value otherStrictlyTerminal = !productions.contains(otherNext);
        value thisStrictlyTerminal = !productions.contains(next);

        if (otherStrictlyTerminal != thisStrictlyTerminal) {
            if (otherStrictlyTerminal) { return larger; }
            return smaller;
        }

        if (other.lsd != lsd) { return lsd.compare(other.lsd); }
        if (other.tokensProcessed != tokensProcessed) { return
            other.tokensProcessed.compare(tokensProcessed); }

        /* Most of the important comparison is done now. */

        value otherToGo = other.rule.consumes.size - other.matchPos;
        value toGo = rule.consumes.size - matchPos;

        return toGo.compare(otherToGo);
    }

    shared actual String string {
        String produces = typeAtomCache.resolve(rule.produces).string;
        variable value ret = "``pos``: ``produces`` =>";
        variable value loc = 0;

        for (i in rule.consumes) {
            ret += " ";
            if (loc++ == matchPos) { ret += "*"; }

            ret += typeAtomCache.resolve(i).string;
        }

        if (complete) { ret +=" *"; }

        return ret + " ``lsd``";
    }
}

"A do-nothing annotation class for the `error` annotation"
shared final annotation class GrammarErrorConstructor()
        satisfies OptionalAnnotation<GrammarErrorConstructor, Annotated> {}

"We annotate some methods of a `ParseTree` object to indicate that those
 methods can construct an error version of symbols so we can build error
 reporting into the parse tree."
shared annotation GrammarErrorConstructor errorConstructor() =>
        GrammarErrorConstructor();

"A do-nothing annotation class for the `rule` annotation"
shared final annotation class GrammarRule()
        satisfies OptionalAnnotation<GrammarRule, Annotated> {}

"We annotate methods of a `ParseTree` object to indicate that those methods
 correspond to production rules"
shared annotation GrammarRule rule() => GrammarRule();

"A do-nothing annotation class for the `tokenizer` annotation."
shared final annotation class Tokenizer()
        satisfies OptionalAnnotation<Tokenizer, Annotated> {}

"Methods annotated with `tokenizer` take a sequence and return a token."
shared annotation Tokenizer tokenizer() => Tokenizer();

"Exception thrown when a [[ParseTree]] is ambiguous. [[ParseTree]] subtypes
 which override [[ParseTree.resolveAmbiguity]] may choose not to throw this
 exception."
shared class AmbiguityException() extends Exception("Parser generated ambiguous
                                                     results") {}

"A queue of states, ordered and also prioritized by amount of error"
class StateQueue() {
    value queue = ArrayList<EPState>();
    value states = HashMap<Integer,HashSet<EPState>>();

    shared <Integer->HashSet<EPState>>? latest => states.last;

    variable PriorityQueue<EPState>? recoveryQueue = null;

    "Initialize recovery queue"
    shared void initRecovery(Rule[] rules) {
        if (recoveryQueue exists) { return; }
        recoveryQueue = PriorityQueue<EPState>((x,y) => x.compareRecovery(y,
                    rules));

        assert(exists r = recoveryQueue);

        for (set in states.items) {
            for (item in set) {
                if (! item.complete) { r.offer(item); }
            }
        }
    }

    "Offer an item to this queue"
    shared void offer(EPState state) {
        if (! states.defines(state.pos)) {
            states.put(state.pos, HashSet<EPState>());
        }

        value target = states[state.pos];
        assert(exists target);

        if (target.contains(state)) { return; }

        target.add(state);

        queue.offer(state);

        if (state.complete) { return; }

        if (exists r=recoveryQueue) {
            r.offer(state);
        }
    }

    "Accept an item from this queue"
    shared EPState? accept() => queue.accept();

    "Get states for a given position"
    shared HashSet<EPState> at(Integer pos) {
        if (! states.defines(pos)) { return HashSet<EPState>(); }
        assert(exists ret = states[pos]);
        return ret;
    }

    "Accept a recovery state"
    shared EPState acceptRecoveryState() {
        assert(exists r=recoveryQueue);
        assert(exists ret=r.accept());
        return ret;
    }
}

"A `ParseTree` is defined by a series of BNF-style production rules. The rules
 are specifed by defining methods with the `rule` annotation.  The parser will
 create an appropriate production rule and call the annotated method in order
 to reduce the value."
shared abstract class ParseTree<out Root, in Data>(Data data)
        given Data satisfies List<Object>
        given Root satisfies Object {
    "A list of rules for this object"
    shared variable Rule[] rules = [];

    "The result symbol we expect from this tree"
    shared Integer result = typeAtomCache.getAlias(`Root`);

    "Error constructors"
    value errorConstructors = HashMap<Integer, Object(Object?)>();

    value tokenCache = HashMap<Integer, Set<Token>>();

    "Tokenizers"
    variable HashMap<Integer, Token?(Data, Object?)> tokenizers =
    HashMap<Integer, Token?(Data, Object?)>();

    "Queue of states to process"
    value stateQueue = StateQueue();

    "Process queued states"
    void pumpStateQueue() {
        while(exists next = stateQueue.accept()) {
            if (next.complete) {
                completeState(next);
            } else {
                propagateState(next);
            }
        }
    }

    "Process a complete state"
    void completeState(EPState state) {
        value prev = stateQueue.at(state.start);
        for (s in prev) {
            if (s.complete) { continue; }
            value n = s.feed(state);

            if (exists n) { stateQueue.offer(n); }
        }
    }

    "Get tokens at a given location"
    Set<Token> getTokens(Integer loc, Object? last) {
        assert(loc <= data.size);

        if (loc == data.size) {
            return HashSet{elements={eos};};
        }

        if (tokenCache.defines(loc)) {
            assert(exists cached = tokenCache[loc]);
            return cached;
        }

        assert(is Data tail = data[loc...]);
        value ret = HashSet{elements={ for (t in tokenizers.items)
            if (exists r = t(tail, last)) r};};

        tokenCache.put(loc, ret);
        return ret;
    }

    "Propagate a state"
    void propagateState(EPState state) {
        Symbol? symbol;

        assert(exists want = state.rule.consumes[state.matchPos]);

        if (exists t = tokenizers[want],
            is Data tail = data[state.pos...],
            exists sym = t(tail, state.lastToken)) {
            symbol = sym;
        } else {
            symbol = null;
        }

        for (s in state.propagate(rules, symbol)) {
            stateQueue.offer(s);
        }
    }

    "Recover an error"
    void recoverError() {
        stateQueue.initRecovery(rules);
        value state = stateQueue.acceptRecoveryState();
        value tokens = getTokens(state.pos, state.lastToken);
        value badToken = tokens.size == 0;

        if (badToken) {
            variable value i = state.pos + 1;

            while (getTokens(i, state.lastToken).size == 0) { i++; }

            assert(is Data tokenData = data[state.pos..(i - 1)]);
            value tok = constructBadToken(tokenData, state.lastToken);

            for (s in state.failPropagate({tok}, true)) {
                stateQueue.offer(s);
            }
        } else {
            value posSet = HashSet<Integer>{elements={ for (t in tokens)
                t.length + state.pos };};
            assert(exists maxPos = max(posSet));
            value resultSet = HashSet<Token>{elements={ for (t in
                    tokens) t };};

            for (i in (state.pos + 1)..(maxPos - 1)) {
                if (posSet.contains(i)) { continue; }

                value toks = getTokens(i, state.lastToken);

                for (tok in toks) {
                    if (posSet.contains(tok.length + state.pos)) {
                        continue;
                    }

                    assert(is Data tokenData = data[state.pos..(i - 1)]);
                    value bad = constructBadToken(tokenData, state.lastToken);

                    resultSet.add(bad);
                    posSet.add(i);
                    break;
                }
            }

            for (s in state.failPropagate(resultSet, false)) {
                stateQueue.offer(s);
            }
        }
    }

    "Confirm that we have successfully parsed."
    Root? validate() {
        assert(exists endsPair = stateQueue.latest);

        value eosTokens = getTokens(endsPair.key, null);

        if (eosTokens.size != 1) {
            recoverError();
            return null;
        }

        /* TODO: Error handling (trailing tokens) */
        assert(exists eosToken = eosTokens.first);

        if (eosToken.sym != eosObject) {
            recoverError();
            return null;
        }

        value resultNodes = ArrayList<Root>();

        for (i in endsPair.item) {
            if (! i.complete) { continue; }
            if (i.rule.produces != result) { continue; }

            assert(is Root t = i.astNode.sym);
            resultNodes.add(t);
        }

        if (resultNodes.size == 1) {
            assert(exists ret = resultNodes[0]);
            return ret;
        }

        if (resultNodes.size > 1) {
            return resolveAmbiguity(resultNodes);
        }

        recoverError();
        return null;
    }

    "The root node of the parse tree"
    shared Root ast {
        if (rules.size == 0) { populateRules(); }
        variable Root? ret = null;

        while (! ret exists) {
            pumpStateQueue();
            ret = validate();
        }

        assert(exists v=ret);
        return v;
    }

    "Method to resolve parse ambiguities. The default implementation simply
     throws [[AmbiguityException]]. Child classes may override this behavior.
     If the child class would like to recover the error, it should return
     a single root node which will be used as the resolved root."
    shared default Root resolveAmbiguity({Object *} roots) {
        throw AmbiguityException();
    }

    "Set up the list of rules"
    void populateRules() {
        value meths = _type(this).getMethods<Nothing>(`GrammarRule`);
        value errConMeths =
            _type(this).getMethods<Nothing>(`GrammarErrorConstructor`);
        value tokenizerMeths = _type(this).getMethods<Nothing>(`Tokenizer`);

        for (t in tokenizerMeths) {
            Token? tokenizer(Data s, Object? last) {
                assert(is Token? ret = t.declaration.memberInvoke(this, [],
                            s, last));
                return ret;
            }

            assert(is UnionType retType = t.type);
            value caseTypes =  retType.caseTypes;
            assert(caseTypes.size == 2);
            assert(is Generic tokenType = {for (r in caseTypes) if (
                        !r.typeOf(null)) r}.first);

            value typeArgs = tokenType.typeArguments.items;
            assert(typeArgs.size == 1);
            assert(exists type = typeArgs.first);

            tokenizers.put(typeAtomCache.getAlias(type), tokenizer);
        }

        for (c in errConMeths) {
            value type = typeAtomCache.getAlias(c.type);

            Object construct(Object? o) {
                assert(is Object ret = c.declaration.memberInvoke(this, [],
                            o));
                return ret;
            }

            errorConstructors.put(type, construct);
        }

        for (r in meths) {
            Object consume(Object *o) {
                assert(is Object ret = r.declaration.memberInvoke(this, [], *o));
                return ret;
            }

            value consumes = [ for (p in r.parameterTypes)
                typeAtomCache.getAlias(p) ];
            value produces = typeAtomCache.getAlias(r.type);
            value rule = Rule(consume, consumes, produces);

            rules = rules.withTrailing(rule);

            if (rule.produces != result) { continue; }

            value newState = EPState(0, rule, 0, 0, [], 0,
                    errorConstructors, 0);
            stateQueue.offer(newState);
        }
    }

    Token constructBadToken(Data data, Object? previous) {
        return Token(badTokenConstructor(data, previous), data.size);
    }

    shared default Object badTokenConstructor(Data data, Object? previous) {
        throw BadTokenConstructorException();
    }
}
