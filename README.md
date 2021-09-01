# rekisteritietopoyta

# Shiny dashboard

App.R lukee rds objektit ja tuottaa kuvaajat, jotka on määritelty ui-osion koodissa. Tiedostosta tilasto_filt_yli1_vastvaihtoeht.rds tunnistetaan rekisterit, joista tehdään lista. Tämä lista näytetään pudotusvalikkona tietopöydässä, mistä käyttäjä valitsee haluamansa laaturekisterin, josta haluaa tarkempaa tietoa. Kun rekisteri on valittu, tietopöydälle tulee toinen pudotusvalikko, josta voi valita rekisteriin liittyvän lomakkeen. Tietopöydässä on kolme välilehteä, jotka näkyvät sivun yläreunassa.

Server-osio koodista tekee lomakekohtaisen määrän kuvaajia riippuen kysymysten määrästä sekä kysymykseen liittyvistä kysymysvaihtoehdoista, jotka näytetään interaktiivisella tietopöydällä.

Yleiset tiedot -välilehdellä on kaksi infolaatikkoa, joissa kerrotaan potilaiden ja täytettyjen lomakkeiden määrä valitussa rekisterissä sekä kolme kuvaajaa, joissa näytetään vuosittaiset määrät potilaista, kokonaismäärät lomakkeista ja eriteltyinä lomakkeittain.

Lomakekohtaiset tiedot -välilehdellä näytetään valitun lomakkeen kysymyksistä ja niiden vastausvaihtoehdoista piirretyt kuvaajat. Miten vastausten kokonaismäärät jakautuvat eri vastausvaihtoehtojen välillä.

Lomakekohtaiset tiedot vuosittain -välilehdellä näytetään valitun lomakkeen kymysten vastausvaihtoehtojen määrien jakauma vuosittain pinotun pylväsdiagrammin avulla.

![Etusivu](https://user-images.githubusercontent.com/89851080/131655227-8ab1c008-f478-4734-9055-a8e8bc0120cd.png)

![lomakekohtaiset_kuvaajat](https://user-images.githubusercontent.com/89851080/131655233-52994a9e-71df-4c33-9eb8-4abac1c2c5d5.png)

![lomakekohtaiset_kuvaajat_vuosittain](https://user-images.githubusercontent.com/89851080/131655238-1aafd371-b341-441f-b911-8dae85b1d14a.png)
