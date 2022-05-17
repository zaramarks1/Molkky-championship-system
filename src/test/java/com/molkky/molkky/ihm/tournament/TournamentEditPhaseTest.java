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
        String randomNbRounds = "5";
        String randomNbCounts = "1";
        String randomNbPlayersPerTeam = "3";

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

        Select select5 = new Select(config.getDriver().findElement(new By.ByName("phases[4].phaseType")));
        select5.selectByIndex(4);

        config.getDriver().findElement(new By.ById("choosePhases")).click();


    }


    @Test
    void testAllDisplay(){

       // String idTournament = config.getDriver().findElement(new By.ByName("phases[0].tournament")).getAttribute("value");

        Assertions.assertEquals("Éditer les informations de la/des phase(s)", config.getDriver().getTitle());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByClassName("contentTitle")).isDisplayed());
        Assertions.assertEquals("Veuillez editer les différentes informations de la/des phase(s)",config.getDriver().findElement
                (new By.ByClassName("contentTitle")).getText());

        Assertions.assertTrue(config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[1]/strong")).isDisplayed());

        Assertions.assertEquals("Phase n°1 de type FINNISH",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[1]/strong")).getText());

        Assertions.assertEquals("Phase n°2 de type KNOCKOUT",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[2]/strong")).getText());

        Assertions.assertEquals("Phase n°3 de type POOL",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[3]/strong")).getText());

        Assertions.assertEquals("Phase n°4 de type SIMPLEGAME",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[4]/strong")).getText());

        Assertions.assertEquals("Phase n°5 de type SWISSPOOL",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[5]/strong")).getText());

        //FINNISH
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].nbFinnish")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].ranking")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].topSeeds")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].notifBeginningPhase")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].nbTeamsQualified")).isDisplayed());



        //knockout
       // Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].nbMatch")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].nbSets")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].ranking")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].topSeeds")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].randomDraw")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].notifEveryRound")).isDisplayed());


        //POOL
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].nbPools")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].nbSets")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].victoryValue")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].ranking")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].topSeeds")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].playTeamSameClub")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].notifBeginningPhase")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].notifEachDay")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].nbTeamsQualified")).isDisplayed());

        //SIMPLE GAME
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].nbSets")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].ranking")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].topSeeds")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].notifBeginningPhase")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].consolation")).isDisplayed());

        //SWISS

        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].nbSets")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].victoryValue")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].nbTeamsQualified")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].notifBeginningPhase")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].nbSubRounds")).isDisplayed());

        Assertions.assertTrue(config.getDriver().findElement(new By.ById("sendPhases")).isDisplayed());

        Assertions.assertEquals("Affectations terrains",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[1]/div[1]/div[1]/label")).getText());
        Assertions.assertEquals("Gestion du planning",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[1]/div[1]/div[4]/label")).getText());
        Assertions.assertEquals("Nombre de finlandaises",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[1]/div[2]/div/div/div[1]/label")).getText());

    }

    @Test
    void testAllDisplayHidden(){

        // String idTournament = config.getDriver().findElement(new By.ByName("phases[0].tournament")).getAttribute("value");

        Assertions.assertEquals("Éditer les informations de la/des phase(s)", config.getDriver().getTitle());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByClassName("contentTitle")).isDisplayed());
        Assertions.assertEquals("Veuillez editer les différentes informations de la/des phase(s)",config.getDriver().findElement
                (new By.ByClassName("contentTitle")).getText());

        //FINNISH

        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].scoreMode")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].consolation")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].playoff")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].terrainAffectation")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].managePlanning")).isDisplayed());


        config.getDriver().findElement(new By.ByName("phases[0].consolation")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].numberConsolationQualify")).isDisplayed());

        config.getDriver().findElement(new By.ByName("phases[0].playoff")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].numberPlayoffQualify")).isDisplayed());

        config.getDriver().findElement(new By.ByName("phases[0].terrainAffectation")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].nbCourts")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].numStartCourt")).isDisplayed());


        config.getDriver().findElement(new By.ByName("phases[0].managePlanning")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].hourPhaseStart")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].timePhase")).isDisplayed());

        //knockout
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].terrainAffectation")).isDisplayed());
        config.getDriver().findElement(new By.ByName("phases[1].terrainAffectation")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].nbCourts")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].numStartCourt")).isDisplayed());

        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].managePlanning")).isDisplayed());
        config.getDriver().findElement(new By.ByName("phases[1].managePlanning")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].hourPhaseStart")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].timePhase")).isDisplayed());

        //POOL

        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].consolation")).isDisplayed());
        config.getDriver().findElement(new By.ByName("phases[2].consolation")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].numberConsolationQualify")).isDisplayed());

        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].playoff")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].terrainAffectation")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].managePlanning")).isDisplayed());

        config.getDriver().findElement(new By.ByName("phases[2].playoff")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].numberPlayoffQualify")).isDisplayed());

        config.getDriver().findElement(new By.ByName("phases[2].terrainAffectation")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].nbCourts")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].numStartCourt")).isDisplayed());


        config.getDriver().findElement(new By.ByName("phases[2].managePlanning")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].hourPhaseStart")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[2].timePhase")).isDisplayed());

        //SIMPLE GAME

        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].consolation")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].terrainAffectation")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].managePlanning")).isDisplayed());

        config.getDriver().findElement(new By.ByName("phases[3].terrainAffectation")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].nbCourts")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].numStartCourt")).isDisplayed());


        config.getDriver().findElement(new By.ByName("phases[3].managePlanning")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].hourPhaseStart")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[3].timePhase")).isDisplayed());


        //SWISS
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].playoff")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].terrainAffectation")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].managePlanning")).isDisplayed());

        config.getDriver().findElement(new By.ByName("phases[4].playoff")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].numberPlayoffQualify")).isDisplayed());

        config.getDriver().findElement(new By.ByName("phases[4].terrainAffectation")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].nbCourts")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].numStartCourt")).isDisplayed());


        config.getDriver().findElement(new By.ByName("phases[4].managePlanning")).click();
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].hourPhaseStart")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[4].timePhase")).isDisplayed());

    }




    @Test
    void insertPhasesValues(){

        String idTournament = config.getDriver().findElement(new By.ByName("phases[0].tournament")).getAttribute("value");


        config.getDriver().findElement(new By.ByName("phases[0].nbFinnish")).sendKeys("4");
        config.getDriver().findElement(new By.ByName("phases[0].ranking")).click();
        config.getDriver().findElement(new By.ByName("phases[0].nbTeamsQualified")).sendKeys("10");

        config.getDriver().findElement(new By.ByName("phases[0].scoreMode")).isDisplayed();
        config.getDriver().findElement(new By.ByName("phases[0].consolation")).isDisplayed();
        config.getDriver().findElement(new By.ByName("phases[0].playoff")).isDisplayed();
        config.getDriver().findElement(new By.ByName("phases[0].terrainAffectation")).isDisplayed();
        config.getDriver().findElement(new By.ByName("phases[0].managePlanning")).isDisplayed();

        //knockout
        config.getDriver().findElement(new By.ByName("phases[1].nbSets")).sendKeys("3");
        //config.getDriver().findElement(new By.ByName("phases[1].nbMatch")).sendKeys("3");

        //pool
        config.getDriver().findElement(new By.ByName("phases[2].nbPools")).sendKeys("2");
        config.getDriver().findElement(new By.ByName("phases[2].nbTeamsQualified")).sendKeys("4");
        config.getDriver().findElement(new By.ByName("phases[2].nbSets")).sendKeys("3");
        config.getDriver().findElement(new By.ByName("phases[2].victoryValue")).sendKeys("2");

        //simple
        config.getDriver().findElement(new By.ByName("phases[3].nbSets")).sendKeys("3");

        //swiss

        config.getDriver().findElement(new By.ByName("phases[4].nbSets")).sendKeys("2");
        config.getDriver().findElement(new By.ByName("phases[4].victoryValue")).sendKeys("2");
        config.getDriver().findElement(new By.ByName("phases[4].nbTeamsQualified")).sendKeys("4");
        config.getDriver().findElement(new By.ByName("phases[4].nbSubRounds")).sendKeys("2");

        config.getDriver().findElement(new By.ById("sendPhases")).click();

        Tournament tournament = tournamentRepository.findById(Integer.valueOf(idTournament));

        List<Phase> phases = phaseRepository.findByTournament(tournament);


        Assertions.assertEquals( 5,phases.size(),"There should be 5 phases");
        Assertions.assertTrue(phases.get(0) instanceof Finnish, "The phase should be finnish");
        Assertions.assertTrue(phases.get(1) instanceof Knockout, "The phase should be knockout");
        Assertions.assertTrue(phases.get(2) instanceof Pool, "The phase should be pool");
        Assertions.assertTrue(phases.get(3) instanceof SimpleGame, "The phase should be simple game");
        Assertions.assertTrue(phases.get(4) instanceof SwissPool, "The phase should be swiss");


        Assertions.assertNotNull(tournament);

    }
    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }



}
