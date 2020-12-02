# Projektno-sufinansiranje-medija

Dobrodošli na github repozitorijum sa kog možete da preuzmete kod u programskom jeziku R i podatke za izradu veb aplikacije projektnog sufinansiranja medijskih sadržaja koja se nalazi na sledećoj [veb adresi](https://centarzaodrzivezajednice.shinyapps.io/Projektno_sufinansiranje_medija_u_Srbiji/). Ova veb aplikacija je izrađena kroz projekat [Centra za održive zajednice](https://odrzivezajednice.org/) "Otvorenim podacima do kvalitetnijeg projektnog sufinansiranja medijskih sadržaja" koju si podržali [Ministarstvo kulture i informisanja Republike Srbije](https://www.kultura.gov.rs/) i [Misija OEBS-a u Srbiji](https://www.osce.org/mission-to-serbia). Takođe, ovde možete preuzeti i kod za izradu online publikacije "Otvorenim podacima do kvalitetnijeg projektnog sufinansiranja medijskih sadržaja" koja je dostupna na sledećem [linku](https://projektnosufinansiranjehtmlizvestaj.netlify.app/). To je još jedan način na koji želimo da podržimo potpunu transparentnost i reproducibilnost istraživanja u ovoj oblasti sa krajnjim ciljem da i drugi mogu doprineti poboljšanju i aplikacije i same analize. Sigurni smo da zajedničkim naporima sa otvorenim pristupom možemo ovaj proces učiniti značajno boljim.

Podaci i kod su slobodni za preuzimanje pod licencom [Creative Commons Zero v1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/). Iako je ovo najotvorenija moguća licenca, zaista bismo voleli da, ako budete koristili podatke ili kod za veb aplikaciju, nas o tome [obavestite](mailto:medijskikonkursi@gmail.com) kako bi mogli da pratimo njenu dalju upotrebu i razvoj, i za to vam se unapred zahvaljujemo. 

## Veb aplikacija

Fajlovi koji su vam potrebni za izradu aplikacije su:
- Projektno sufinansiranje medija.xlsx
- app.R
- folder www

## Online publikacija "Otvorenim podacima do kvalitetnijeg projektnog sufinansiranja medijskih sadržaja"

Fajlovi koji su vam potrebni za izradu online publikacije su:
- Projektno sufinansiranje medija.xlsx
- population.xlsx
- Index.Rmd
- Index.html

## Metodološke napomene

Excel file – Projektno sufinansiranje medija.xlsx sadrži sledeće kolone:


1. **ORGAN KOJI RASPISUJE KONKURS/OPŠTINA** – odnosi se na instituciju koja je raspisala konkurs: Ministarstvo kulture i informisanja, Pokrajinski sekretarijat za kulturu, javno informisanje i odnose sa verskim zajednicama i 152 lokalne samouprave (gradovi i opštine). Pošto gradske opštine nisu u obavezi da raspisuju konkurse za projektno sufinansiranje, navedene su samo one gradske opštine koje su makar jednom objavile konkurs ( za Beograd to su Lazarevac, Mladenovac i Obrenovac, a za Niš – Pantelej, Crveni Krst i Medijana).

2. **GODINA** – kolona koja se odnosi na godinu kada je konkurs raspisan.

3. **PODNOSILAC PROJEKTA** – Ova kolona beleži nazive podnosilaca projekata (preduzeća, agencije i drugo) kako su registrovani u Agenciji za privredne registre (APR). Podaci su proveravani na [sajtu APR-a](https://www.apr.gov.rs). Zbog neujednačenog korišćenja velikih i malih slova u registru APR-a, svi podnosioci projekata su ispisani verzalom (velikim slovima). U nekim situacijama podnosilac projekta predstavlja instituciju poput fakulteta ili religijske organizacije koja nije registrovana u APR-u. Njihov naziv je, u tom slučaju, preuziman iz rešenja samog konkursa.

4. **MATIČNI BROJ PODNOSIOCA** – Ova kolona se nalazi samo u podacima koji se mogu skinuti u excel i csv formatu. Matični broj je preuzet sa sajta APR-a. Pošto se među podnosiocima projekata nalaze i institucije poput Fakulteta političkih nauka ili Saveza jevrejskih opština Sombora, uneti su matični brojevi iz registara kojima oni pripadaju. Takođe, postoji slučaj jednog podnosioca (PRIVREDNO DRUŠTVO UNIVERSAL COMPANY, LAZAREVAC, DOO) za koga na APR-ovom sajtu ne postoji informacija o matičnom broju, a postoje i neki podnosioci koji su registrovani na teritoriji AP Kosovo i Metohija kojih nema u registru i oni su zavedeni pod šifrom **11111111**. Ukoliko su podnosioci entiteti registrovani izvan granica Republike Srbije, dodeljena im je šifra **22222222**.

5. **NAZIV MEDIJA** – Ova kolona se odnosi na naziv medija kome su dodeljena sredstva. Podsećamo da postoji zaseban registar medija koji se vodi kod APR-a, i da mediji nemaju svojstvo pravnog lica, već to ima njihov osnivač (podnosilac projekta). Nazivi medija su takođe usaglašavani sa bazom APR-a. Ukoliko su finansirani stručni skupovi, konferencije ili radionice i to je bilo jasno naznačeno u nazivu projekta – ova kolona je ostavljena prazna. Ukoliko nije data informacija kojem mediju su data sredstva za proizvodnju sadržaja preuzet je ključ iz nekih rešenja konkursa poput Pokrajinskog sekretarijata i upisivana je reč “Produkcija” u ovu kolonu. U situacijama kada je izdavač medija vlasnik samo jednog medija prema APR-u, a nije naveden naziv medija u rešenju konkursa, stavljan je naziv tog medija iz APR-a.

6. **NAZIV PROJEKTA** – Ova kolona se popunjava ukoliko se nazivi projekata nalaze u rešenjima. Ukoliko oni nisu navedeni, polje ostaje prazno. Ukoliko je konkurs poništen, konkurs nije raspisan ili sredstva nisu dodeljena sve je podvedeno pod stavku – sredstva nisu dodeljena. U slučaju konkursa za AP Kosovo i Metohiju i konkursa za informisanje na srpskom jeziku van teritorije Republike Srbije – ta informacija je dodata u zagradu odmah posle naziva projekta.

7. **TEMA PROJEKTA** – Teme projekata su izvođene iz njihovih naziva, tamo gde su oni bili navedeni. Identifikovali smo i klasifikovali osam tematskih okvira: ekologija i zdravstvo, ekonomija, informativni program, kultura i obrazovanje, neprivilegovane grupe, ostalo, manjinski sadržaj i sport. Te teme prate definiciju javnog interesa iz Zakona o javnom informisanju i medijima. Tamo gde nije bilo naziva projekta, ova kolona je ostala nepopunjena. Određeni broj naziva projekata koji ne potpadaju ni pod jednu od navedenih tema zavedeni su pod – Ostalo. Jasno je da ovo nije savršen metodološki pristup i da bi jasno definisanje tematskog okvira bilo moguće odrediti tek kroz evaluaciju projekata. Dakle, u pitanju je indikacija tema, koja po nama može da prikaže određene trendove. Nadamo se da će rezultati našeg projekta omogućiti istraživačima lakši rad na dubljim i specifičnijim analizama konkursnog sufinansiranja.

- Ekologija i zdravstvo

Ova tema široko obuhvata sve što se odnosi na zaštitu životne sredine i zaštitu zdravlja.

- Ekonomija

Ova tema je široko obuhvatila poljoprivredu, turizam, ekonomski segment evropskih integracija i razvoj sela.

- Informativni program

Ova tema je obuhvatila proizvodnju informativnog programa. Iako je u nekim slučajevima sufinansiranje informativnog programa bilo opravdano, ostaje utisak da u velikom broju slučajeva nije bio finansiran zakonom definisan javni interes, već redovna medijska produkcija. Ovo može takođe da bude tema za dublju analizu.

- Kultura i obrazovanje

Ova tema je široko obuhvatila kulturne i razne edukativne sadržaje, uključujući i organizaciju stručnih skupova.

- Manjinski sadržaj

Ova tema je obuhvatila ne samo proizvodnju sadržaja na manjinskom jeziku već i sadržaj koji govori o određenim manjinama i njihovoj kulturi. Svi medijski sadržaji na manjinskim jezicima su stavljani u “manjinsku kategoriju” iako su i oni, naravno, tematski raznorodni. Cilj takvog pristupa je bilo tretiranje informisanja na manjinskim jezicima kao zasebnog fenomena – sa ciljem da se može posmatrati na koji način se država brine o njemu. Naravno, i na ovu temu preporučujemo dalja, dublja istraživanja čija osnova mogu da budu – podaci i web aplikacija nastali u okviru realizacije ovog projekta.

- Neprivilegovane grupe

Ova tema je široko postavljena da obuhvati razne grupe, poput osoba sa invaliditetom, dijabetesom, multiplesklerozom, ali i grupe kao što su – žene, mladi, stari i deca. Ova klasifikacija je isuviše široko postavljena i ona bi se u daljem radu mogla podeliti na određene podteme.

- Sport

Ova tema obuhvata proizvodnju sportskih medijskih sadržaja.

- Ostalo

U ovu temu su ušle podteme poput verskih i religioznih, kao i tema kao što je povećanje nataliteta. U ovu kategoriju su klasifikovane i teme koje nisu bile dovoljno jasno definisane da bi mogle nedvosmisleno da se podvedu pod bilo koju gore pomenutu oblast.

8. **SREDSTVA U DINARIMA** – Ova kolona predstavlja nominalne vrednosti koje su date određenom podnosiocu projekta na osnovu rešenja za dodelu sredstava. Zbog toga što su ove vrednosti nominalne, ne koriste se za poređenja po godinama i kumulativne prikaze u grafikonima, već se za to koristi kolona SREDSTVA U EVRIMA.

9. **SREDSTVA U EVRIMA** – Ova kolona je obračunata na osnovu srednjeg godišnjeg kursa evra za svaku godinu od 2015-2019. i srednjeg mesečnog kursa za avgust za 2020. godinu, a podaci su uzeti sa [sajta Narodne banke Srbije](https://www.nbs.rs/sr_RS/finansijsko_trziste/medjubankarsko-devizno-trziste/kursna-lista/prosecni-kursevi/index.html). Iz tog razloga se ova kolona koristi za poređenja po godinama i kumulativne vrednosti u grafikonima i tabelama. Istraživači su svesni da bi realističniji prikaz predstavljalo korišćenje realnih vrednosti koje bi se preračunale na osnovu indeksa potrošačkih cena, ali zbog lakšeg razumevanja celokupne tabele, odlučeno je da se za to ipak koristi srednji kurs evra.
