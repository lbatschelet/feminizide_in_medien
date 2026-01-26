```
Projektname: Medienberichterstattung über Feminizide
Mitarbeitende: Lukas Batschelet, Linus Kiener
Seminar: Computational Social Science
Betreuende: Rudy Farys, Achim Edelmann
```

## Idee.
Unser Projekt hat zum Ziel, Erkenntnisse über die Berichterstattung zu Feminiziden zu ermöglichen. Durch die Swissdox@LiRI API verschaffen wir
uns (regulär) Zugang zu zahlreichen Medienbeiträgen in der Schweiz, die
in einem weiteren Schritt durch verschiedene packages und Instrumente in
R ausgewertet werden könn(t)en. 

## Detail.
Die Swissdox@LiRI API wird über R abgerufen. Sollte dies funktionieren, verzichten wir auf Webscraping/Datamining. Der API werden einerseits
rohe Textdaten, mit denen linguistische Analysen vorgenommen werden können,
und andererseits Metadaten entnommen, damit die Daten sinnvoll gefiltert
werden können.


Meeting Minutes (19.01.2026, 14:00):

- Bericht
    Umfang und Tiefe
    technische Anforderungen

    -> "es wäre spannend gewesen, die Berichterstattung gegen
    die tatsächlichen (offiziellen) Daten abzugelichen, doch das ist nicht möglich"

    -> es existieren Guerillia/open-intelligence seiten, doch auch diese Daten sind (natürlicher Weise) unvollständig. (siehe z.B. stopfemizid.ch)

    -> Ethik dahinter evtl. erwähnen
 
- Code
    nur API: limits, dokumentation, etc.\
    Analysen: wird Wert darauf gelegt\
    Begriffe, etc.\
    eigene Analysen erforschen\
    similarity analysen, co-ocurrence


- Daten (insb. Aufbereitung)
    Deutsch; cleaning\
    haben wir alle 'Begriffe' erfasst?\
    machen wir mehrere Loops?\
    wie machen wir die Filterung?

    wie erfassen wird das Phänomen im letzten Jahrhundert?\
    wurde darüber berichtet?\
    in welchem Umfang?

    doppelte Artikel wegen Zentralredaktionen


- Analysen
    Pre-processing:

    word2vec (max. 100 dimensionen)
    was ist in der Nähe von was?
    
    https://www.geeksforgeeks.org/r-machine-learning/word2vec-using-r/
    "In conclusion, Word2Vec, employing CBOW and Skip-Gram models, generates powerful word embeddings by capturing semantic relationships. CBOW predicts a target word from its context, while Skip-Gram predicts context words from a target word. These embeddings enable NLP tasks and, as demonstrated with airline reviews, showcase the models' ability to find contextually similar words."

    https://medium.com/@manansuri/a-dummys-guide-to-word2vec-456444f3c673
    "What we then observe is:
    As expected, “king”, “queen”, “prince” have similar scores for “royalty” and “girl”, “queen” have similar scores for “femininity”.
    An operation that removes “man” from “king”, would yield in a vector very close to “queen” ( “king”- “man” = “queen”)
    Vectors “king” and “prince” have the same characteristics, except for age, telling us how they might possibly be semantically related to each other.

    ---

    doc2vec

    https://cran.r-project.org/web/packages/doc2vec/readme/README.html
    This repository contains an R package allowing to build Paragraph Vector models also known as doc2vec models. You can train the distributed memory (‘PV-DM’) and the distributed bag of words (‘PV-DBOW’) models. Next to that, it also allows to build a top2vec model allowing to cluster documents based on these embeddings. (max. 1000 Wörter pro Doc!)


---

    
evtl. racism und sexism (synonym) dictionary\
Netzwerk-Analyse\
2 to 1 node projection (evtl. weniger interessant)\
'ownership' über Begriffe

WiDiD (Word Identity Detection): An approach for detecting semantic shifts through incremental, scalable clustering.

Semantic Shift Detection (SSD): Researchers use computational models, particularly transformer-based models like BERT, to detect when a word's meaning changes across different time periods.
https://www.uibk.ac.at/en/disc/blog/change-in-word-meaning/#:~:text=28.04.2022,their%20chosen%20words%20%5B1%5D.

evtl. Google n-gram?

---

<!-- für uns -->

## Integrating the bibliography in Quarto

Pandoc supports bibliographies in a wide variety of formats including BibTeX and CSL. Add a bibliography to your document using the bibliography YAML metadata field. For example:

```
---
title: "My Document"
bibliography: library.bib
link-citations: true
---
```

See the Pandoc Citations documentation for additional information on bibliography formats.

### In-text citation

```
Blah Blah [see @doe99, pp. 33-35; also @smith04, chap. 1].
Blah Blah [@doe99, pp. 33-35, 38-39 and passim].
Blah Blah [@smith04; @doe99].
Smith says blah [-@smith04].

@smith04 says blah.
@smith04 [p. 33] says blah.
```


