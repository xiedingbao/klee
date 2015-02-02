class Foo{}
;

int main(int argc, char* argv[]){
    Foo *p = new Foo[4];
    delete p;
    return 0;
}
