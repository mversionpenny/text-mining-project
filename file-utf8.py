import sys
from unidecode import unidecode

def main(argv):
    iFile = argv[0]
    oFile = argv[1]
    with open(iFile, "rb") as f:
        text = unidecode(f.read().decode('utf-8'))
    with open(oFile, "w") as p:
        p.write(text)

if __name__ == "__main__":
    print sys.argv[1:]
    main(sys.argv[1:])
