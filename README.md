Projektname: Medienberichterstattung über Feminizide
Mitarbeitende: Lukas Batschelet, Linus Kiener
Seminar: Computational Social Science
Betreuende: Rudy Farys, Achim Edelmann

Idee.
Unser Projekt hat zum Ziel, Erkenntnisse über die Berichterstattung zu Feminiziden zu ermöglichen. Durch die Swissdox@LiRI API verschaffen wir
uns (regulär) Zugang zu zahlreichen Medienbeiträgen in der Schweiz, die
in einem weiteren Schritt durch verschiedene packages und Instrumente in
R ausgewertet werden könn(t)en. 

Detail.
Die Swissdox@LiRI API wird über R abgerufen. Sollte dies funktionieren, verzichten wir auf Webscraping/Datamining. Der API werden einerseits
rohe Textdaten, mit denen linguistische Analysen vorgenommen werden können,
und andererseits Metadaten entnommen, damit die Daten sinnvoll gefiltert
werden können.


Meeting Minutes (19.01.2026, 14:00):

- Bericht
    Umfang und Tiefe
    technische Anforderungen
    -> "es wäre spannend gewesen, die Berichterstattung gegen
    die tatsächlichen (offiziellen) Daten abzugelichen, doch
    das ist nicht möglich"
    -> es existieren Guerillia/open-intelligence seiten, doch 
    auch diese Daten sind (natürlich) unvollständig.
    stopfemizid.ch 
    -> Ethik dahinter
 
- Code
    nur API: limits, dokumentation, etc.
    Analysen: wird Wert darauf gelegt
    Begriffe, etc.
    eigene Analysen erforschen
    similarity analysen, co-ocurrence

    Pre-processing:
    wordtovec (max. 100 dimensionen)
    was ist in der Nähe von was?
    evtl. racism und sexism (synonym) dictionary
    Netzwerk-Analyse
    2 to 1 node projection (evtl. weniger interessant)
    'ownership' über Begriffe


- Daten (insb. Aufbereitung)
    Deutsch; cleaning
    haben wir alle 'Begriffe' erfasst?
    machen wir mehrere Loops?
    wie machen wir die Filterung?

    wie erfassen wird das Phänomen im letzten Jahrhundert?
    wurde darüber berichtet?
    in welchem Umfang?

    doppelte Artikel wegen Zentralredaktionen
