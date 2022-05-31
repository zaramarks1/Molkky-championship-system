package com.molkky.molkky.ihm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.checkerframework.checker.units.qual.A;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import type.UserRole;

import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class AllTournamentFormTest {
    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;
    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;

    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
        if(userRepository.findUserByEmail("pierre.admin@test.com")==null) {
            Tournament tournament = new Tournament();
            tournament.setName("tournamentAdminTest");
            tournament.setVisible(true);
            tournamentRepository.save(tournament);
            User admin = new User();
            admin.setEmail("pierre.admin@test.com");
            admin.setPassword("test");
            userRepository.save(admin);
            UserTournamentRole userTournamentRoleAdmin = new UserTournamentRole();
            userTournamentRoleAdmin.setRole(UserRole.ADM);
            userTournamentRoleAdmin.setUser(admin);
            userTournamentRoleAdmin.setTournament(tournament);
            userTournamentRoleRepository.save(userTournamentRoleAdmin);
        }
        config.getDriver().get(url+"/connexion");
        config.getDriver().findElement(new By.ById("email")).sendKeys("pierre.admin@test.com");
        config.getDriver().findElement(new By.ById("password")).sendKeys("test");
        config.getDriver().findElement(new By.ById("connexion")).click();
        Assertions.assertEquals("Accueil", config.getDriver().getTitle());
        config.getDriver().get(url+"/tournament/allTournament");
    }

    @Test
    void testAllTournamentGetPage() {
        config.getDriver().get(url+"/tournament/allTournament");
        Assertions.assertEquals("Affichage des tournois", config.getDriver().getTitle());
    }

    @Test
    void testAllBoutonIsDisplayed() {
        config.getDriver().get(url+"/tournament/allTournament");
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("valider")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("inscription")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("open")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("closed")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("inProgress")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("valider")).isDisplayed());
    }

    @Test
    void testAllAttributesDisplayed() {
        config.getDriver().get(url+"/tournament/allTournament");
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
        config.getDriver().get(url+"/tournament/allTournament");
        config.getDriver().findElement(new By.ById("open")).click();
        Assertions.assertTrue(config.getDriver().getCurrentUrl().endsWith("tournament/TournamentOpen"));
        config.getDriver().findElement(new By.ById("closed")).click();
        Assertions.assertTrue(config.getDriver().getCurrentUrl().endsWith("tournament/TournamentClose"));
        config.getDriver().findElement(new By.ById("inProgress")).click();
        Assertions.assertTrue(config.getDriver().getCurrentUrl().endsWith("tournament/TournamentInProgress"));
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertTrue( config.getDriver().getCurrentUrl().contains("tournament/create"));
    }

    @Test
    void testAllTournament() {
        List<WebElement> nbPage = config.getDriver().findElements(new By.ByClassName("boxOneCard"));
        int nbBDD = tournamentRepository.findAll().size();
        Assertions.assertEquals(nbPage.size(), nbBDD);
    }


    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }

}
