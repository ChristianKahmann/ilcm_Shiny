#Test File Write performance


#Test DB Performance

#NLP
#cleanNLP - spacy,
-- tibble, id



1. Schreiben der Token Tabelle nach DB
1.1 Tabelle in Insert vs. Infile

2. Lesen der Token aus DB
2.1 Resultset komplett zu Zieldatenstruktur
2.2 Resultset Stream abarbeiten -- jeweils zu Datenstruktur
2.3 Resultset stream zu komplett Zieldatenstruktur

3. Annotation (Literaturrecherche)
3.1 Speichern als Tupel mit referenz auf Wort
3.2 Speichern als Anhang an Wort selbst (Update auf DB Zeile)
3.3 Lesen aus Worttabelle selbst
3.4 Lesen aus eigener Tabelle und anfügen an tibble
 
 
Alternativen: Paralellzugriff, Parrallel mit beiläufigem kontruieren des REsults
 
 
Implementierung mit testthat

CleanNLP
In Rmd Docker mit MAriaDB starten
Tabellen anlegen
Tests durchführen
Docker Obkjekt löschen

NLP
In Rmd Docker mit MAriaDB starten
Tabellen anlegen
Tests durchführen
Docker Obkjekt löschen

CLEANNLP vs. NLP (openNLP) -- Qualität -- Testdaten, engl. deutsch.
-- Goldstandard: POS Tags, Parse Baum, Token, Sätze
-- Erweiterbarkeit
-- Laufzeit abhängig von Qualität


Annotationen
In Rmd Docker mit MAriaDB starten
Tabellen anlegen
Tests durchführen
Docker Obkjekt löschen

Guardian