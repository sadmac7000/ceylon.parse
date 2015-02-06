"A string of bad data"
class Crap(shared String data, shared Integer position = 0) {
    shared actual Integer hash = data.hash;
    shared actual Boolean equals(Object that) {
        if (is Crap that) {
            return that.data == this.data;
        }

        return false;
    }
    shared variable Boolean consumed = false;

    shared actual String string {
        return "``position``-``data``";
    }
}
