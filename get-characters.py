from unidecode import unidecode
import sys
def main(argv):
    iFile = argv[0]
    oFile = argv[1]
    cleanr = re.compile('<br />')
    tree = ET.parse(iFile)
    root = tree.getroot()
    persons = root.findall(".//span[@id='Person']")
    characters = open(oFile, 'w')
    arrayOfPersons = []
    for person in persons:
        personOk = re.sub(cleanr, '', person.text)
        personOk = personOk.encode('utf-8')
        arrayOfPersons.append(personOk)
        arrayAntiDoubles = []
    for name in arrayOfPersons:
        if arrayOfPersons.count(name) > 2 and name not in arrayAntiDoubles:
            arrayAntiDoubles.append(name)
            characters.write(name + "\n")


if __name__ == "__main__":
    print sys.argv[1:]
    main(sys.argv[1:])