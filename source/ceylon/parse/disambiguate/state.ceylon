import ceylon.collection {
    ArrayList,
    MutableSet,
    MutableMap,
    HashMap,
    HashSet
}

import ceylon.parse { Rule, Atom, ProductionClause }

class RuleSet(shared Rule[] rules, shared Atom start) {
    class PositionedRule(shared Rule rule, shared Integer pos) {
        shared actual Integer hash = rule.hash ^ 2 + pos;
        shared actual Boolean equals(Object that) {
            if (is PositionedRule that) {
                return that.rule == rule && that.pos == pos;
            }

            return false;
        }

        shared ProductionClause? current => rule.consumes[pos];

        shared Boolean matches_empty {
            value cur = current;

            if (! exists cur) { return false; }
            assert(exists cur);

            return cur.contains(Atom(`Null`)) || (cur.variadic && !cur.once);
        }

        shared {PositionedRule *} derivatives {
            if (pos == rule.consumes.size) { return {}; }

            value ret = PositionedRule(rule, pos + 1);

            if (! ret.matches_empty) { return {ret}; }
            return {ret}.chain(ret.derivatives);
        }
    }

    class ExtendedRule(shared Rule rule,
            shared State final,
            shared [State+] transitions) {
        shared State initial => transitions[0];
    }

    class State(MutableSet<PositionedRule> initSet) {
        value queue = ArrayList{ *initSet };

        while (exists prule = queue.accept()) {
            assert(exists current = prule.current);
            for (atom in current) {
                for (r in rules) {
                    if (! r.produces.subtypeOf(atom)) { continue; }

                    value newRule = PositionedRule(r, 0);

                    if (initSet.contains(newRule)) { continue; }

                    initSet.add(newRule);
                    queue.offer(newRule);
                    break;
                }
            }
        }

        shared Set<PositionedRule> prules = initSet;

        shared actual Integer hash;

        value hashVar = rules.reduce<Integer>((x,y) {
            if (is Integer x) { return x ^ 2 + y.hash; }
            else { return x.hash ^ 2 + y.hash; }
        });

        if (! exists hashVar) { hash = 0; }
        else if (is Integer hashVar) { hash = hashVar; }
        else { hash = hashVar.hash; }

        shared actual Boolean equals(Object other) {
            if (is State other) {
                return other.prules == prules;
            }

            return false;
        }

        shared MutableMap<Atom, State> transitions = HashMap<Atom, State>();

        value transitionSets = HashMap<Atom, MutableSet<PositionedRule>>();

        for (s in prules) {
            value current = s.current;
            if (! exists current) { continue; }
            assert(exists current);

            for (atom in current) {
                if (atom == Atom(`Null`)) { continue; }

                if (! transitionSets.defines(atom)) {
                    transitionSets.put(atom, HashSet<PositionedRule>());
                }

                assert(exists target = transitionSets[atom]);
                for (k in s.derivatives) { target.add(k); }
            }
        }

        for (k->v in transitionSets) {
            transitions.put(k,State(v));
        }

        shared Set<ExtendedRule> exRules {
            class ExRuleCursor(PositionedRule rule, State[] path = []) {
                shared {ExRuleCursor*} next {
                    value current = rule.current;
                    value results = ArrayList<ExRuleCursor>();

                    if (! exists current) { return {}; }
                    assert(exists current);

                    for (a in current) {
                        if (a == Atom(`Null`)) { continue; }

                        Map<Atom,State> trans;

                        if (path.size == 0) {
                            trans = transitions;
                        } else {
                            assert(exists l = path.last);
                            trans = l.transitions;
                        }

                        assert(exists next = trans[a]);
                        results.add(ExRuleCursor(PositionedRule(rule.rule,
                                        rule.pos + 1),
                                    path.withTrailing(next)));
                    }

                    if (rule.matches_empty) {
                        results.add(ExRuleCursor(PositionedRule(rule.rule,
                                        rule.pos + 1), path));
                    }

                    return results;
                }

                shared ExtendedRule? result {
                    if (rule.current exists) {
                        return null;
                    }

                    value final = transitions[rule.rule.produces];
                    assert(exists final);
                    assert(nonempty path);
                    return ExtendedRule(rule.rule, final, path);
                }
            }

            value queue = ArrayList<ExRuleCursor>();
            value result = HashSet<ExtendedRule>();

            for (r in prules) {
                if (r.pos != 0) { continue; }
                queue.add(ExRuleCursor(r, []));
            }

            while (exists cursor = queue.accept()) {
                if (exists res = cursor.result) {
                    result.add(res);
                }

                queue.addAll(cursor.next);
            }

            return result;
        }
    }

    State startState = State(HashSet{for (r in rules)
        if (r.produces == start) PositionedRule(r, 0)});

    value queue = ArrayList{startState};
    value stateSet = HashSet<State>();

    while (exists state = queue.accept()) {
        stateSet.add(state);

        for (k->v in state.transitions) {
            if (! stateSet.contains(v)) {
                queue.offer(v);
                continue;
            }

            for (s in stateSet) {
                if (s != v) { continue; }

                state.transitions.put(k, s);
                break;
            }
        }
    }
}
