# Instruktioner på svenska
Svenska Dagbladet publicerar varje dag ett ord-pussel vid namn Dagens Nia. Man ges nio bokstäver, som omkastade bildar
ett svenskt ord på nio bokstäver. Pusslet går ut på att hitta vilket ord det är, eller vilka ord, då det kan finnas fler
än en lösning.

Vår bot niancat hjälper till att kontrollera om ett ord finns med i ordlistan, och om den matchar dagens pussel eller
inte. I övrigt håller den koll på vilka som har löst nian, och i vilken ordning de gjorde det. Den varnar också om det
finns fler än en lösning på dagens nian, så man vet hur många lösningar man kan hitta.
Dessutom kan man spara roliga ord (som vi kallar olösningar) som man hittar.

## Att lösa nian
Varje morgon i #konsulatet så skriver en person (vanligtvis Erik) in Dagens Nia. Då får man veta:

- Dagens bokstäver
- Om det finns mer än en lösning så nämns det.
- Gårdagens lösning, och vilka som löste den
- En lista på alla olösningar som folk sparar.

Vill du veta vilket dagens pussel är, använd kommandot `!nian`, antingen i #konsulatet eller i ett privatmeddelande till
niancat.

Vill du testa en lösning, skriv in ordet i ett direktmeddelande till niancat. Om ordet är korrekt så kommer niancat
svara det, och notifiera #konsulatet att du har löst nian. Om ordet inte är korrekt kommer den svara att ordet inte
finns med i ordlistan.

Om du skrivit in fel bokstäver så svarar niancat med vilka bokstäver som inte matchar pusslet.

Skriv helst inte in försök till lösningar i den publika kanalen, då man kan råka spoila dagens pussel för andra, även
om man inte tror lösningen är nära den rätta. Spara istället roliga ord som olösningar, så publiceras de automatiskt
nästa dag.

## Kommandon
Vill du testa en lösning skriver du ordet i ett privatmeddelande till niancat. Du kan skriva in den med både små och
stora bokstäver, och mellanslag och bindestreck kommer automatiskt tas bort innan den jämförs med dagens pussel.

- !nian

    Niancat svarar med dagens pussel. Fungerar både i publika kanalen och i privatmeddelande.

- !sättnian \<pussel\>

    Sätt dagens pussel. Kan göras både publikt och i privat-kanal.
    Niancat kommer skriva ut följande information i #konsulatet:
    + Dagens pussel som just sattes.
    + Antalet lösningar till dagens pussel, om det finns mer än en lösning.
    + Gårdagens lösning(ar) och vilka som löste den.
    + En lista på alla sparade olösningar.
     
    Exempel: !sättnian STOPLEDAR

- !olösning \<fritext\>

    Spara rolig text som publiceras automatiskt nästa dag.
    Niancat svarar att olösningen är sparad.
    
    Används bara i privatmeddelande till niancat.
    
    Fritexten kan vara vad som helst, men om inget ord i fritexten matchar dagens pussel så kommer du behöva bekräfta
    olösningen. Du kan bekräfta den genom att direkt:
    + Skriva in samma olösning igen, eller
    + Använda kommandot !olösning utan någon fritext.
    
    Exempel: !olösning LEDARPOST fanns med i ordlistan, men gör inte det längre.

- !olösning

    Använd detta kommando, utan någon fritext för att bekräfta en tidigare olösning. Om du inte har någon tidigare
    olösning som behöver bekräftas så kommer niancat svara det.
    
    Används bara i privatmeddelande till niancat.

- !olösningar

    Skriv ut vilka olösningar du har sparat.

    Används bara i privatmeddelande till niancat.

## Beta-version
Ibland uppdaterar vi boten, och då kopplar den upp sig som boten tiancat istället, och sitter i kanalen #betacat
istället för #konsulatet. Där testar vi nya features.