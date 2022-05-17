package com.molkky.molkky.ihm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class AllTournamentFormTest {
    @Autowired
    private TournamentRepository tournamentRepository;
    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;

    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s/tournament/allTournament", port.toString());
    }

    @Test
    void testAllTournamentGetPage() {
        config.getDriver().get(url);
        Assertions.assertEquals("Affichage des tournois", config.getDriver().getTitle());
    }

    @Test
    void testAllBoutonIsDisplayed() {
        config.getDriver().get(url);
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("open")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("closed")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("inProgress")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("valider")).isDisplayed());
    }

    @Test
    void testAllAttributesDisplayed() {
        config.getDriver().get(url);
        List<WebElement> nbTournois = config.getDriver().findElements(new By.ByClassName("boxOneCard"));
        for (int i = 0; i <nbTournois.size(); i++) {
            Assertions.assertTrue(nbTournois.get(i).findElement(new By.ByClassName("nameCard")).isDisplayed());
            Assertions.assertTrue(nbTournois.get(i).findElement(new By.ByClassName("dateCard")).isDisplayed());
            Assertions.assertTrue(nbTournois.get(i).findElement(new By.ByClassName("nbCard")).isDisplayed());
            Assertions.assertTrue(nbTournois.get(i).findElement(new By.ByClassName("textPlayer")).isDisplayed());
            Assertions.assertTrue(nbTournois.get(i).findElement(new By.ByClassName("textPlayer")).isDisplayed() || nbTournois.get(i).findElement(new By.ByClassName("textInscriptionOpen")).isDisplayed());
        }
    }

    @Test
    void testButtonTypeTournament() {
        config.getDriver().get(url);
        config.getDriver().findElement(new By.ById("open")).click();
        Assertions.assertTrue(config.getDriver().getCurrentUrl().endsWith("tournament/TournamentOpen"));
        config.getDriver().findElement(new By.ById("closed")).click();
        Assertions.assertTrue(config.getDriver().getCurrentUrl().endsWith("tournament/TournamentClose"));
        config.getDriver().findElement(new By.ById("inProgress")).click();
        Assertions.assertTrue(config.getDriver().getCurrentUrl().endsWith("tournament/TournamentInProgress"));
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertTrue( config.getDriver().getCurrentUrl().endsWith("tournament/create"));
    }

    @Test
    void testAllTournament() {
        config.getDriver().get(url);
        List<WebElement> nbPage = config.getDriver().findElements(new By.ByClassName("boxOneCard"));
        int nbBDD = tournamentRepository.findAll().size();
        Assertions.assertEquals(nbPage.size(), nbBDD);
    }


    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }

}
