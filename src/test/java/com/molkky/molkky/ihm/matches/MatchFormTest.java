package com.molkky.molkky.ihm.matches;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.MatchService;
import org.apache.commons.lang.RandomStringUtils;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import type.UserRole;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Random;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class MatchFormTest {
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;
    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private MatchService matchService;
    @Autowired
    private SetRepository setRepository;
    @Autowired
    private CourtRepository courtRepository;
    @Autowired
    private RoundRepository roundRepository;



    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
    }

    @Test
    void testAccessUnlogged(){
//        given
        Match match = createCompleteMatch();
//        when
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
//        then
        Assertions.assertEquals(url + "/connexion", config.getDriver().getCurrentUrl());
    }

    @Test
    void testMatchFormDisplayedLogged(){
//        given
        Match match = createCompleteMatch();
        loginUser(match.getTeams().get(0).getUserTournamentRoles().get(0).getUser());
        Tournament tournament = match.getRound().getTournament();
        DateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
        String strDate = dateFormat.format(tournament.getDate());
//        when
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
//        then
        Assertions.assertEquals(url + "/matches/match?match_id=" + match.getId(), config.getDriver().getCurrentUrl());
        Assertions.assertEquals(tournament.getName(), config.getDriver().findElement(By.className("tournamentTitle")).getText());
        Assertions.assertEquals(strDate, config.getDriver().findElement(By.className("tournamentDate")).getText());
        Assertions.assertEquals(match.getCourt().getName(), config.getDriver().findElement(By.className("courtText")).getText());
        Assertions.assertEquals(match.getFinished() ? "" : "En cours", config.getDriver().findElement(By.className("stateText")).getText());
        Assertions.assertEquals("Jeu en " + match.getNbSets() + " sets", config.getDriver().findElement(By.className("bestOfText")).getText());
    }

    @Test
    void testInsertScoreTeam1(){
//        given
        Match match = createCompleteMatch();
        loginUser(match.getTeams().get(0).getUserTournamentRoles().get(0).getUser());
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
        int score1 = new Random().nextInt(50);
        int score2 = new Random().nextInt(50);
//        when
        config.getDriver().findElement(By.name("score1Team1")).clear();
        config.getDriver().findElement(By.name("score1Team1")).sendKeys(Integer.toString(score1));
        config.getDriver().findElement(By.name("score2Team1")).clear();
        config.getDriver().findElement(By.name("score2Team1")).sendKeys(Integer.toString(score2));
        config.getDriver().findElement(By.id("submitSet0")).click();
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
//        then
        Assertions.assertEquals(Integer.toString(score1), config.getDriver().findElement(By.name("score1Team1")).getAttribute("value"));
        Assertions.assertEquals(Integer.toString(score2), config.getDriver().findElement(By.name("score2Team1")).getAttribute("value"));
    }

    @Test
    void testInsertScoreTeam2(){
//        given
        Match match = createCompleteMatch();
        loginUser(match.getTeams().get(1).getUserTournamentRoles().get(0).getUser());
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
        int score1 = new Random().nextInt(50);
        int score2 = new Random().nextInt(50);
//        when
        config.getDriver().findElement(By.name("score1Team2")).clear();
        config.getDriver().findElement(By.name("score1Team2")).sendKeys(Integer.toString(score1));
        config.getDriver().findElement(By.name("score2Team2")).clear();
        config.getDriver().findElement(By.name("score2Team2")).sendKeys(Integer.toString(score2));
        config.getDriver().findElement(By.id("submitSet0")).click();
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
//        then
        Assertions.assertEquals(Integer.toString(score1), config.getDriver().findElement(By.name("score1Team2")).getAttribute("value"));
        Assertions.assertEquals(Integer.toString(score2), config.getDriver().findElement(By.name("score2Team2")).getAttribute("value"));
    }

    @Test
    void testInsertScoreTeamOrga(){
//        given
        Match match = createCompleteMatch();
        User user = createOrgaUser();
        loginUser(user);
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
        int score1 = new Random().nextInt(50);
        int score2 = new Random().nextInt(50);
//        when
        config.getDriver().findElement(By.name("score1Orga")).clear();
        config.getDriver().findElement(By.name("score1Orga")).sendKeys(Integer.toString(score1));
        config.getDriver().findElement(By.name("score2Orga")).clear();
        config.getDriver().findElement(By.name("score2Orga")).sendKeys(Integer.toString(score2));
        config.getDriver().findElement(By.id("submitSet0")).click();
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
//        then
        Assertions.assertEquals(Integer.toString(score1), config.getDriver().findElement(By.name("score1Orga")).getAttribute("value"));
        Assertions.assertEquals(Integer.toString(score2), config.getDriver().findElement(By.name("score2Orga")).getAttribute("value"));
    }

    void loginUser(User user){
        config.getDriver().get(url + "/connexion");
        config.getDriver().findElement(new By.ById("email")).sendKeys(user.getEmail());
        config.getDriver().findElement(new By.ById("password")).sendKeys(user.getPassword());
        if(user.getUserTournamentRoles().get(0).getRole() != UserRole.STAFF){
            config.getDriver().findElement(new By.ById("teamCode")).sendKeys(user.getUserTournamentRoles().get(0).getTeam().getCode());
        }
        config.getDriver().findElement(new By.ById("connexion")).click();
    }

    User createOrgaUser(){
        UserTournamentRole userTournamentRole1 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user1 = new User();
        user1.setEmail(RandomStringUtils.randomAlphabetic(10) + "@gmail.com");
        user1.setPassword(RandomStringUtils.randomAlphabetic(10));
        user1.setUserTournamentRoles(List.of(userTournamentRole1));
        userRepository.save(user1);
        userTournamentRole1.setUser(user1);
        userTournamentRole1.setRole(UserRole.STAFF);
        userTournamentRoleRepository.save(userTournamentRole1);
        return user1;
    }

    Match createCompleteMatch() {
        Match match = matchRepository.save(new Match());
        Team team1 = teamRepository.save(new Team());
        team1.setCode(RandomStringUtils.randomAlphabetic(10));
        Team team2 = teamRepository.save(new Team());
        team2.setCode(RandomStringUtils.randomAlphabetic(10));

        UserTournamentRole userTournamentRole1 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user1 = new User();
        user1.setEmail(RandomStringUtils.randomAlphabetic(10) + "@gmail.com");
        user1.setPassword(RandomStringUtils.randomAlphabetic(10));
        userRepository.save(user1);
        userTournamentRole1.setUser(user1);
        userTournamentRole1.setTeam(team1);
        userTournamentRole1.setRole(UserRole.PLAYER);
        userTournamentRoleRepository.save(userTournamentRole1);
        team1.setUserTournamentRoles(List.of(userTournamentRole1));
        team1 = teamRepository.save(team1);

        UserTournamentRole userTournamentRole2 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user2 = userRepository.save(new User());
        user2.setEmail(RandomStringUtils.randomAlphabetic(10) + "@gmail.com");
        user2.setPassword(RandomStringUtils.randomAlphabetic(10));
        userRepository.save(user2);
        userTournamentRole2.setUser(user2);
        userTournamentRole2.setTeam(team2);
        userTournamentRole2.setRole(UserRole.PLAYER);
        userTournamentRoleRepository.save(userTournamentRole2);
        team2.setUserTournamentRoles(List.of(userTournamentRole2));
        team2 = teamRepository.save(team2);

        Set set1 = setRepository.save(new Set());
        set1.setMatch(match);
        set1.setTeams(List.of(team1, team2));
        setRepository.save(set1);

        match.setCourt(courtRepository.save(new Court(true, "court")));
        match.setSets(List.of(set1));
        match.setTeams(Arrays.asList(team1, team2));
        match.setNbSets(1);
        match = matchRepository.save(match);

        Tournament tournament = tournamentRepository.save(new Tournament());
        tournament.setName(RandomStringUtils.randomAlphabetic(10));
        tournament.setDate(Date.from(Instant.now()));

        Round round = new Round();
        round.setTournament(tournament);
        roundRepository.save(round);
        tournament.setRounds(List.of(round));
        round.setMatches(List.of(match));
        match.setRound(round);
        tournamentRepository.save(tournament);
        roundRepository.save(round);
        matchRepository.save(match);

        return match;
    }
    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}

