Funkcionální projekt 1 do FLP - PLG2NKA - 2021  
Spuštení: plg-2-nka volby [vstup]  
    vstup - jméno vstupního soubor, pokud není zadáno je vstup brán ze stdin  
    volby -i na výstup je vypsána vstupní gramatika z vnitřní reprezentace  
          -1 na vystup je vypsána tranfsormovaná gramatika dle věty 3.2 (opora TINu)  
          -2 na výstup je vypsán výsledný automat dle věty 3.6 (opora TINu)  

Program čísluje nově vytvořené neterminály pomocí jedné posloupnosti, takže nově vznikné neterminály mají tvar A1,A2,B3,C4,A5 atd.  

Program kontroluje správnost vstupní gramatiky a pokud nesplňuje následující, tak je vypsán error:  
    -neterminály,terminály a pravidla jsou množiny (tj neobsahují duplicity)  
    -neterminály a terminály jsou dlouhé max 1 znak a jsou z A-Z resp. a-z  
    -pravidla se skládají pouze z terminálů a neterminálů obsažených ve vstupní množině  
    -pravidla neobsahují jednoduchá pravidla a jsou pouze "pravá" tj ve tvaru Nonterm->termsNonterm  
