package com.molkky.molkky.ihm.tournament;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.*;
import com.molkky.molkky.repository.PhaseRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.support.ui.Select;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

import java.text.ParseException;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
 class TournamentEditPhaseTest {

    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private PhaseRepository phaseRepository;

    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;

    @BeforeAll
    void setUp()  throws ParseException {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
    }

    @BeforeEach
    void configTournament(){

        config.getDriver().get(url + "/tournament/create");
        String randomName = "Tournoi " + Math.floor(Math.random() * 100000);
        String randomLocation = "location de test";
        String randomDateTournoi = "01/01/2020";
        String randomCutOffDate = "01/01/2020";
        String randomMinTeam = "5";
        String randomMaxTeam = "20";
        String randomNbRounds = "4";
        String randomNbCounts = "1";
        String randomNbPlayersPerTeam = "3";
        String randomMail = "test@gmail.com";

        config.getDriver().findElement(new By.ById("nom")).sendKeys(randomName);
        config.getDriver().findElement(new By.ById("location")).sendKeys(randomLocation);
        config.getDriver().findElement(new By.ById("dateTournoi")).sendKeys(randomDateTournoi);
        config.getDriver().findElement(new By.ById("cutOffDate")).sendKeys(randomCutOffDate);
        config.getDriver().findElement(new By.ById("nbPlayersPerTeam")).sendKeys(randomNbPlayersPerTeam);
        config.getDriver().findElement(new By.ById("minTeam")).sendKeys(randomMinTeam);
        config.getDriver().findElement(new By.ById("maxTeam")).sendKeys(randomMaxTeam);
        config.getDriver().findElement(new By.ById("visible")).click();
        config.getDriver().findElement(new By.ById("nbRounds")).sendKeys(randomNbRounds);
        config.getDriver().findElement(new By.ById("nbCourts")).sendKeys(randomNbCounts);
        config.getDriver().findElement(new By.ById("email")).sendKeys(randomMail);
        config.getDriver().findElement(new By.ById("sendTournament")).click();

        Assertions.assertEquals("Choix de la/des phase(s)", config.getDriver().getTitle());
        Select select = new Select(config.getDriver().findElement(new By.ByName("phases[0].phaseType")));
        select.selectByIndex(0);

        Select select2 = new Select(config.getDriver().findElement(new By.ByName("phases[1].phaseType")));
        select2.selectByIndex(1);

        Select select3 = new Select(config.getDriver().findElement(new By.ByName("phases[2].phaseType")));
        select3.selectByIndex(2);

        Select select4 = new Select(config.getDriver().findElement(new By.ByName("phases[3].phaseType")));
        select4.selectByIndex(3);

        config.getDriver().findElement(new By.ById("choosePhases")).click();
    }

    @Test
    void testAllDisplay(){

        Assertions.assertEquals("Éditer les informations de la/des phase(s)", config.getDriver().getTitle());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByClassName("contentTitle")).isDisplayed());
        Assertions.assertEquals("Veuillez éditer les différentes informations de la/des phase(s)",config.getDriver().findElement
                (new By.ByClassName("contentTitle")).getText());

        Assertions.assertTrue(config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[1]/strong")).isDisplayed());

        Assertions.assertEquals("Phase n°1 de type POOL",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[1]/strong")).getText());

        Assertions.assertEquals("Phase n°2 de type SWISSPOOL",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[2]/strong")).getText());

        Assertions.assertEquals("Phase n°3 de type KNOCKOUT",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[3]/strong")).getText());

        Assertions.assertEquals("Phase n°4 de type SIMPLEGAME",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[4]/strong")).getText());

        //KNOCKOUT
        testPool(0);

        //POOL
        testSwiss(1);

        //SIMPLE GAME
        testKnockout(2);

        //SWISS
        testSimpleGame(3);

        Assertions.assertTrue(config.getDriver().findElement(new By.ById("sendPhases")).isDisplayed());
    }

    void testKnockout(int index){
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].nbSets")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].ranking")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].topSeeds")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].randomDraw")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].notifEveryRound")).isDisplayed());
    }

    void testPool(int index){
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].nbPools")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].nbSets")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].victoryValue")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].ranking")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].topSeeds")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].playTeamSameClub")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].notifBeginningPhase")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].nbTeamsQualified")).isDisplayed());
    }

    void testSimpleGame(int index){
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].nbSets")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].ranking")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].topSeeds")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].notifBeginningPhase")).isDisplayed());
    }

    void testSwiss(int index){
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].nbSets")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].victoryValue")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].nbTeamsQualified")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].notifBeginningPhase")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[" + index + "].nbSubRounds")).isDisplayed());
    }


    void verifyKnockout(){
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].randomDraw")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].notifEveryRound")).isDisplayed());
    }

    void verifyPool(){
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].playTeamSameClub")).isDisplayed());
    }

    void verifySimplegame(){
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].notifBeginningPhase")).isDisplayed());
    }

    void verifySwiss(){
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].nbTeamsQualified")).isDisplayed());
    }

    @Test
    void testAllDisplayHidden(){

        Assertions.assertEquals("Éditer les informations de la/des phase(s)", config.getDriver().getTitle());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByClassName("contentTitle")).isDisplayed());
        Assertions.assertEquals("Veuillez éditer les différentes informations de la/des phase(s)",config.getDriver().findElement
                (new By.ByClassName("contentTitle")).getText());

        verifyPool();
        verifyKnockout();
        verifySimplegame();
        verifySwiss();
    }

    @Test
    void insertPhasesValues(){

        String idTournament = config.getDriver().findElement(new By.ByName("phases[0].tournament")).getAttribute("value");

        //knockout
        config.getDriver().findElement(new By.ByName("phases[2].nbSets")).sendKeys("3");

        //pool
        config.getDriver().findElement(new By.ByName("phases[0].nbPools")).sendKeys("2");
        config.getDriver().findElement(new By.ByName("phases[0].nbTeamsQualified")).sendKeys("4");
        config.getDriver().findElement(new By.ByName("phases[0].nbSets")).sendKeys("3");
        config.getDriver().findElement(new By.ByName("phases[0].victoryValue")).sendKeys("2");

        //simple
        config.getDriver().findElement(new By.ByName("phases[3].nbSets")).sendKeys("3");
        config.getDriver().findElement(new By.ByName("phases[3].nbTeamsQualified")).sendKeys("1");

        //swiss
        config.getDriver().findElement(new By.ByName("phases[1].nbSets")).sendKeys("2");
        config.getDriver().findElement(new By.ByName("phases[1].victoryValue")).sendKeys("2");
        config.getDriver().findElement(new By.ByName("phases[1].nbTeamsQualified")).sendKeys("4");
        config.getDriver().findElement(new By.ByName("phases[1].nbSubRounds")).sendKeys("2");

        config.getDriver().findElement(new By.ById("sendPhases")).click();

        Tournament tournament = tournamentRepository.findById(Integer.valueOf(idTournament));

        List<Phase> phases = phaseRepository.findByTournament(tournament);

        Assertions.assertEquals( 4,phases.size(),"There should be 4 phases");
        Assertions.assertTrue(phases.get(2) instanceof Knockout, "The phase should be knockout");
        Assertions.assertTrue(phases.get(0) instanceof Pool, "The phase should be pool");
        Assertions.assertTrue(phases.get(3) instanceof SimpleGame, "The phase should be simple game");
        Assertions.assertTrue(phases.get(1) instanceof SwissPool, "The phase should be swiss");


        Assertions.assertNotNull(tournament);

    }
    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }



}
