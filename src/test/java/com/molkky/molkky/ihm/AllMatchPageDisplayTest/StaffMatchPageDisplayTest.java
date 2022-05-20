package com.molkky.molkky.ihm.AllMatchPageDisplayTest;


import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.repository.*;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
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

    @Autowired
    private PhaseRepository phaseRepository;

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

            Pool phase = new Pool();
            Round round = new Round();

            //phase.setRounds(List.of(round));
            phase.setTournament(tournament);
            phaseRepository.save(phase);

            round.setPhase(phase);
            round.setTournament(tournament);
            round = roundRepository.save(round);

            //roundRepository.save(round);
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
            else{
                User staff = userRepository.findUserByEmail(emailStaff);
                Tournament old = tournamentRepository.findByName("TournamentConnexion");
                List<UserTournamentRole> utr = userTournamentRoleRepository.findUserTournamentRoleByTournamentAndUser(old,staff);
                utr.get(0).setTournament(tournament);
                userTournamentRoleRepository.save(utr.get(0));
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
        config.getDriver().get(url + "/connexion");
        config.getDriver().findElement(new By.ById("email")).sendKeys(emailStaff);
        config.getDriver().findElement(new By.ById("password")).sendKeys(passwordStaff);
        config.getDriver().findElement(new By.ById("connexion")).click();
        Assertions.assertEquals("Accueil", config.getDriver().getTitle());
        config.getDriver().findElement(new By.ById("matches")).click();
    }


    @Test
    void allMatchStaff(){
        config.getDriver().findElement(new By.ById("matches")).click();
        Assertions.assertEquals("Affichage des Matchs", config.getDriver().getTitle());
        Assertions.assertEquals(url+"/match/allMatches", config.getDriver().getCurrentUrl());
        Assertions.assertEquals("Affichage des Matchs", config.getDriver().getTitle());
        config.getDriver().get(url + "/match/allMatches");
        config.getDriver().findElement(new By.ById("all")).click();
        Assertions.assertEquals(url+"/match/allMatches", config.getDriver().getCurrentUrl());
        int nbMatchBDD = matchRepository.findMatchAttributedToStaff(tournamentRepository.findByName("TournamentTestStaff"),
                userRepository.findUserByEmail(emailStaff)).size();
        if(nbMatchBDD!=0) {
            List<WebElement> nbMatch = config.getDriver().findElements(new By.ById("listMatches"));
            Assertions.assertEquals(nbMatchBDD, nbMatch.size());
            String idDiv = config.getDriver().findElement(new By.ById("idMatchList")).getText();
            String[] div = idDiv.split(" :");
            String id = div[1];
            config.getDriver().findElement(new By.ById("listMatches")).click();
            Assertions.assertEquals("Match en cours", config.getDriver().getTitle());
            Assertions.assertEquals(url + "/matches/match?match_id=" + id, config.getDriver().getCurrentUrl());
        }
    }
    @Test
    void matchInProgressDisplay() {
        config.getDriver().get(url + "/match/allMatches");
        config.getDriver().findElement(new By.ById("inProgress")).click();
        Assertions.assertEquals(url+"/match/inProgressMatches", config.getDriver().getCurrentUrl());
        int nbMatchBDD = matchRepository.findMatchAttributedToStaffAndFinished(tournamentRepository.findByName("TournamentTestStaff"),
                userRepository.findUserByEmail(emailStaff),false).size();
        if(nbMatchBDD!=0) {
            List<WebElement> nbMatch2 = config.getDriver().findElements(new By.ById("listMatches"));
            Assertions.assertEquals(nbMatchBDD, nbMatch2.size());
            String idDiv2 = config.getDriver().findElement(new By.ById("idMatchList")).getText();
            String[] div2 = idDiv2.split(" :");
            String id2 = div2[1];
            config.getDriver().findElement(new By.ById("listMatches")).click();
            Assertions.assertEquals("Match en cours", config.getDriver().getTitle());
            Assertions.assertEquals(url + "/matches/match?match_id=" + id2, config.getDriver().getCurrentUrl());
        }
    }

    @Test
    void matchToCheckDisplay() {
        config.getDriver().get(url + "/match/allMatches");
        config.getDriver().findElement(new By.ById("toCheck")).click();
        Assertions.assertEquals(url+"/match/validateMatch", config.getDriver().getCurrentUrl());
        int nbMatchBDD = matchRepository.findMatchAttributedToStaff(tournamentRepository.findByName("TournamentTestStaff"),
                userRepository.findUserByEmail(emailStaff)).size();
        if(nbMatchBDD!=0) {
            String idDiv3 = config.getDriver().findElement(new By.ById("idMatchList")).getText();
            String[] div3 = idDiv3.split(" :");
            String id3 = div3[1];
            config.getDriver().findElement(new By.ById("listMatches")).click();
            Assertions.assertEquals("Match en cours", config.getDriver().getTitle());
            Assertions.assertEquals(url + "/matches/match?match_id=" + id3, config.getDriver().getCurrentUrl());
        }
    }
    @Test
    void matchFinishedDisplay() {
        config.getDriver().get(url + "/match/allMatches");
        config.getDriver().findElement(new By.ById("closed")).click();
        Assertions.assertEquals(url + "/match/finishedMatches", config.getDriver().getCurrentUrl());
        int nbMatchBDD = matchRepository.findMatchAttributedToStaffAndFinished(tournamentRepository.findByName("TournamentTestStaff"),
                userRepository.findUserByEmail(emailStaff),true).size();
        if(nbMatchBDD!=0) {
            List<WebElement> nbMatch4 = config.getDriver().findElements(new By.ById("listMatches"));
            Assertions.assertEquals(nbMatchBDD, nbMatch4.size());
            String idDiv4 = config.getDriver().findElement(new By.ById("idMatchList")).getText();
            String[] div4 = idDiv4.split(" :");
            String id4 = div4[1];
            config.getDriver().findElement(new By.ById("listMatches")).click();
            Assertions.assertEquals("Match en cours", config.getDriver().getTitle());
            Assertions.assertEquals(url+"/matches/match?match_id="+id4, config.getDriver().getCurrentUrl());
        }

    }
    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}
