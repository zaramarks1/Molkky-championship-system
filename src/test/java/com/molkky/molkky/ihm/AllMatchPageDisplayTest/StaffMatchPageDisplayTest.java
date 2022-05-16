package com.molkky.molkky.ihm.AllMatchPageDisplayTest;


import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.repository.*;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import type.UserRole;

import java.util.ArrayList;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class StaffMatchPageDisplayTest {

    @Autowired
    private UserRepository userRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private CourtRepository courtRepository;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private SetRepository setRepository;
    private SeleniumConfig config;


    @Value("${server.port}")
    private Integer port;
    private String url;
    private String emailStaff = "connexion.staff@gmail.com";
    private String passwordStaff = "staff123";

    private String teamName = "TeamTestStaff1";
    private String teamName2 = "TeamTestStaff2";


    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
        if(tournamentRepository.findByName("TournamentTestStaff")==null) {
            Tournament tournament = new Tournament();
            tournament.setName("TournamentTestStaff");
            tournament.setVisible(true);
            tournamentRepository.save(tournament);
            Team team = new Team();
            team.setName(teamName);
            teamRepository.save(team);
            Team team2 = new Team();
            team2.setName(teamName2);
            teamRepository.save(team2);
            Round round = new Round();
            round.setTournament(tournament);
            roundRepository.save(round);
            Court court = new Court();
            court.setName("courtTestStaff");
            courtRepository.save(court);
            if(userRepository.findUserByEmail(emailStaff)==null) {
                User staff = new User();
                staff.setEmail(emailStaff);
                staff.setPassword(passwordStaff);
                userRepository.save(staff);
                UserTournamentRole userTournamentRoleStaff = new UserTournamentRole();
                userTournamentRoleStaff.setRole(UserRole.STAFF);
                userTournamentRoleStaff.setUser(staff);
                userTournamentRoleStaff.setTournament(tournament);
                userTournamentRoleRepository.save(userTournamentRoleStaff);
            }
            Match match = new Match();
            match.setRound(round);
            match.setCourt(court);
            match.setFinished(false);
            List<Team> teams = new ArrayList<Team>();
            teams.add(team);
            teams.add(team2);
            match.setTeams(teams);
            User staff = userRepository.findUserByEmail(emailStaff);
            /*UserTournamentRole userTournamentRoleStaff3 = new UserTournamentRole();
            userTournamentRoleStaff3.setRole(UserRole.STAFF);
            userTournamentRoleStaff3.setUser(staff);
            userTournamentRoleStaff3.setTournament(tournament2);
            userTournamentRoleRepository.save(userTournamentRoleStaff3);*/
            match.setUser(staff);
            matchRepository.save(match);
            Set set1 = new Set();
            set1.setScore1Team1(10);
            set1.setScore1Team2(35);
            set1.setMatch(match);
            Set set2 = new Set();
            set2.setScore2Team1(25);
            set2.setScore2Team2(8);
            set2.setMatch(match);
            setRepository.save(set1);
            setRepository.save(set2);
        }
    }


    @Test
    void connectStaff(){
        WebDriverWait wait = new WebDriverWait(config.getDriver(), 30);
        config.getDriver().get(url + "/connexion");
        config.getDriver().findElement(new By.ById("email")).sendKeys(emailStaff);
        config.getDriver().findElement(new By.ById("password")).sendKeys(passwordStaff);
        config.getDriver().findElement(new By.ById("connexion")).click();
        wait.until(ExpectedConditions.visibilityOf(config.getDriver().findElement(new By.ById("homeDescription"))));
        Assertions.assertEquals("Accueil", config.getDriver().getTitle());
        config.getDriver().findElement(new By.ById("matches")).click();
        Assertions.assertEquals("Affichage des Matchs", config.getDriver().getTitle());
        Assertions.assertEquals(url+"/match/allMatches", config.getDriver().getCurrentUrl());
        Assertions.assertEquals("Affichage des Matchs", config.getDriver().getTitle());
        //TO DO : Separate tests
        //Test allMatchesPageIsDisplayed
        config.getDriver().findElement(new By.ById("all")).click();
        Assertions.assertEquals(url+"/match/allMatches", config.getDriver().getCurrentUrl());
        WebElement matches = config.getDriver().findElement(new By.ById("listMatches"));
        //Assertions.assertEquals(1, matches.);
        //Test inProgressPageIsDisplayed
        config.getDriver().findElement(new By.ById("inProgress")).click();
        Assertions.assertEquals(url+"/match/inProgressMatches", config.getDriver().getCurrentUrl());
        //Test validatePageIsDisplayed
        config.getDriver().findElement(new By.ById("toCheck")).click();
        Assertions.assertEquals(url+"/match/validateMatch", config.getDriver().getCurrentUrl());
        //Test finishedIsDisplayed
        config.getDriver().findElement(new By.ById("closed")).click();
        Assertions.assertEquals(url+"/match/finishedMatches", config.getDriver().getCurrentUrl());


    }
    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}


