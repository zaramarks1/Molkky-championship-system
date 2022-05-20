package com.molkky.molkky.ihm.TeamForm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.support.ui.Select;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

import java.text.ParseException;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class TeamFormTest {
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private TournamentRepository tournamentRepository;
    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;

    @BeforeAll
    void setUp() throws ParseException {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
        this.createTournament();
    }

    @Test
    void testTournamentFormGetPage() {
        config.getDriver().get(url + "/team/create");
        Assertions.assertEquals("Création d'une nouvelle équipe", config.getDriver().getTitle());
    }

    @Test
    void testFormIsDisplayed(){
        config.getDriver().get(url + "/team/create");
        Assertions.assertTrue(config.getDriver().findElement(new By.ByClassName("contentTitle")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("nom")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("tournament")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("sendTeam")).isDisplayed());

        Assertions.assertEquals("Créer une équipe",config.getDriver().findElement
                (new By.ByClassName("contentTitle")).getText());
        Assertions.assertEquals("Nom de l'équipe",config.getDriver().findElement
                (new By.ByCssSelector("body > div > div.contentContainer > form > div:nth-child(1) > label")).getText());
        Assertions.assertEquals("Selectionnez un tournoi",config.getDriver().findElement
                (new By.ByCssSelector("body > div > div.contentContainer > form > div:nth-child(2) > label")).getText());
        Assertions.assertEquals("Étape suivante",config.getDriver().findElement(new By.ById("sendTeam")).getText());
    }

    @Test
    void testTeamFormAddInfo(){
        config.getDriver().get(url + "/team/create");
        String teamName = "Test" + Math.floor(Math.random() * 100);


        config.getDriver().findElement(new By.ById("nom")).sendKeys(teamName);
        Select select = new Select(config.getDriver().findElement(new By.ById("tournament")));
        select.selectByIndex(select.getOptions().size() - 1);
        String idTournament = config.getDriver().findElement(new By.ById("tournament")).getAttribute("value");
        config.getDriver().findElement(new By.ById("sendTeam")).click();

        Team team = teamRepository.findByName(teamName);

        Assertions.assertNotNull(team,"Team not save");
        Assertions.assertEquals(teamName,team.getName(),"Name different");
        Assertions.assertEquals(idTournament,String.valueOf(team.getTournament().getId()),"IdTournament different");
    }

    void createTournament() throws ParseException {
        TournamentModel tournament = new TournamentModel();
        tournament.setName("Tournoi " + Math.floor(Math.random() * 1000));
        tournament.setLocation("location de test");
        tournament.setDate("2019-01-01");
        tournament.setCutOffDate("2019-01-01");
        tournament.setMinTeam(5);
        tournament.setMaxTeam(20);
        tournament.setNbRounds(1);
        tournament.setNbCourts(1);
        tournament.setNbPlayersPerTeam(2);
        tournament.setVisible(true);

        Tournament tournament1 = new Tournament(tournament);
        tournamentRepository.save(tournament1);
    }


    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}
