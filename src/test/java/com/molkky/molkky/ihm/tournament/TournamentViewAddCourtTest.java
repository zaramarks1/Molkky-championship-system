package com.molkky.molkky.ihm.tournament;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.repository.*;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import type.UserRole;

import java.text.ParseException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import static org.apache.commons.lang.RandomStringUtils.randomAlphabetic;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class TournamentViewAddCourtTest {

    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private CourtRepository courtRepository;
    @Autowired
    private PhaseRepository phaseRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private PhaseRepository PhaseRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;
    private String tournamentName = "TournamentAddStaff"+Math.floor(Math.random() * 1000);
    private String emailAdmin = "admin"+Math.floor(Math.random() * 10000)+randomAlphabetic(15)+"@gmail.com";
    private String passwordAdmin = "pwd"+Math.floor(Math.random() * 100000);
    Date tenDaysAgo = Date.from(Instant.now().minus(Duration.ofDays(10)));
    Date oneDayAfter = Date.from(Instant.now().plus(Duration.ofDays(1)));
    private int minTeam = Math.max(1,(int)(Math.random()*3));
    private int maxTeam = (int)(Math.random()*8);

    private int nbCourts = Math.max(1,(int)(Math.random()*8));
    private int nbTeam = Math.max(1,(int)(Math.random()*5));
    private int nbPhase = (int)(Math.random()*8);




    @BeforeAll
    void setUp()  throws ParseException {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
        Tournament tournament = new Tournament();
        tournament.setName(tournamentName);
        tournament.setVisible(true);
        tournament.setDate(oneDayAfter);
        tournament.setCutOffDate(tenDaysAgo);
        tournament.setLocation("Angers");
        tournament.setMinTeam(minTeam);
        tournament.setMaxTeam(maxTeam);
        tournamentRepository.save(tournament);
        User admin = new User();
        admin.setEmail(emailAdmin);
        admin.setPassword(passwordAdmin);
        userRepository.save(admin);
        UserTournamentRole userTournamentRoleAdmin = new UserTournamentRole();
        userTournamentRoleAdmin.setRole(UserRole.ADM);
        userTournamentRoleAdmin.setUser(admin);
        userTournamentRoleAdmin.setTournament(tournament);
        userTournamentRoleRepository.save(userTournamentRoleAdmin);
        for(int i=0;i<nbTeam;i++) {
            Team team = new Team();
            team.setName("team"+tournamentName+i);
            team.setTournament(tournament);
            teamRepository.save(team);
        }
        for(int i=0;i<nbPhase;i++) {
            Phase phase = new Phase();
            phase.setTournament(tournament);
            PhaseRepository.save(phase);
        }
        config.getDriver().get(url + "/connexion");
        config.getDriver().findElement(new By.ById("email")).sendKeys(emailAdmin);
        config.getDriver().findElement(new By.ById("password")).sendKeys(passwordAdmin);
        config.getDriver().findElement(new By.ById("connexion")).click();
        Assertions.assertEquals("Accueil", config.getDriver().getTitle());
    }

    @Test
    void accessToTournamentView(){
        Tournament tournament = tournamentRepository.findByName(tournamentName);
        int tournamentId = tournament.getId();
        config.getDriver().get(url + "/tournament/view?tournamentId=" + tournamentId);
        Assertions.assertEquals("Votre Tournoi", config.getDriver().getTitle());
        String tournamentNameIHM = config.getDriver().findElement(new By.ById("tournament_name")).getText();
        Assertions.assertEquals(tournament.getName(),tournamentNameIHM);
        String dateLocationIHM = config.getDriver().findElement(new By.ById("date-and-location")).getText();
        String dateAndLocationDB = "Le tournoi "+tournament.getName()+" du "
                +tournament.getDate()+" aura lieu à " + "Angers débutera le " + tournament.getDate()+".";
        Assertions.assertEquals(dateAndLocationDB,dateLocationIHM);
        String nbTeamsIHM = config.getDriver().findElement(new By.ById("teams-extremums")).getText();
        String nbTeamsDB = "Le nombre d'équipes pour ce tournoi est compris entre "
                +tournament.getMinTeam()+" et "+tournament.getMaxTeam()+".";
        Assertions.assertEquals(nbTeamsDB,nbTeamsIHM);
        String nbTeamsRegisteredIHM = config.getDriver().findElement(new By.ById("nbTeam")).getText();
        String nbTeamsRegisteredDB = "Il y a actuellement "+nbTeam+" équipes inscrites à ce tournoi.";
        Assertions.assertEquals(nbTeamsRegisteredDB,nbTeamsRegisteredIHM);
    }

    @Test
    void addCourtTournament(){
        List<String> courtNames = new ArrayList<>();
        for(int i = 0; i< nbCourts; i++){
            courtNames.add(randomAlphabetic(15) + " terrain");
        }
        Tournament tournament = tournamentRepository.findByName(tournamentName);
        int tournamentId = tournament.getId();
        config.getDriver().get(url + "/tournament/view?tournamentId=" + tournamentId);
        config.getDriver().findElement(new By.ById("court-counter")).sendKeys(Integer.toString(nbCourts));
        config.getDriver().findElement(new By.ById("addCourt")).click();
        Assertions.assertEquals("Ajouter un terrain", config.getDriver().getTitle());
        List<WebElement> nbCourtsInputs = config.getDriver().findElements(new By.ById("name"));
        Assertions.assertEquals(nbCourts, nbCourtsInputs.size());

        for(int i = 0; i< nbCourts; i++){
            config.getDriver().findElement(new By.ByName("courts["+i+"].name")).sendKeys(courtNames.get(i));
        }

        config.getDriver().findElement(new By.ById("sendTeam")).click();

        for(int i = 0; i< nbCourts; i++){
            Assertions.assertTrue(courtRepository.existsByName(courtNames.get(i)));
        }

        int nbCourtsAdded = courtRepository.findByTournament(tournament).size();
        Assertions.assertEquals(nbCourts,nbCourtsAdded);
        List<WebElement> nbDisplayedCourts = config.getDriver().findElements(new By.ByClassName("courtCard"));
        Assertions.assertEquals(nbCourts, nbDisplayedCourts.size());
    }

    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }




}
