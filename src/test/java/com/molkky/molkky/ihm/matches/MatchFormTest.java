package com.molkky.molkky.ihm.matches;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.MatchService;
import org.apache.commons.lang.RandomStringUtils;
import org.junit.jupiter.api.*;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import type.PhaseType;
import type.UserRole;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

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
    @Autowired
    private  PhaseRepository phaseRepository;

    public final CountDownLatch count = new CountDownLatch(1);

    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
    }

    @Test
    void testAccessUnlogged() {
//        given
        Match match = createCompleteMatch();
//        when
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
//        then
        Assertions.assertEquals(url + "/matches/match?match_id=" + match.getId(), config.getDriver().getCurrentUrl());
    }

    @Test
    void testMatchFormDisplayedLogged() {
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
       // Assertions.assertEquals(match.getCourt().getName(), config.getDriver().findElement(By.className("courtText")).getText());
        Assertions.assertEquals(match.getFinished() ? "" : "En cours", config.getDriver().findElement(By.className("stateText")).getText());
        Assertions.assertEquals("Jeu en " + match.getNbSets() + " sets", config.getDriver().findElement(By.className("bestOfText")).getText());
    }

    @Test
    void testInsertScoreTeam1() {
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
    void testInsertScoreTeam1WrongScore() {
//        given
        Match match = createCompleteMatch();
        loginUser(match.getTeams().get(0).getUserTournamentRoles().get(0).getUser());
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
        int score1 = -10;
        int score2 = 70;
//        when
        config.getDriver().findElement(By.name("score1Team1")).clear();
        config.getDriver().findElement(By.name("score1Team1")).sendKeys(Integer.toString(score1));
        config.getDriver().findElement(By.name("score2Team1")).clear();
        config.getDriver().findElement(By.name("score2Team1")).sendKeys(Integer.toString(score2));
        config.getDriver().findElement(By.id("submitSet0")).click();
//        then
//        score stays the same because nothing was sent
        Assertions.assertEquals(Integer.toString(score1), config.getDriver().findElement(By.name("score1Team1")).getAttribute("value"));
        Assertions.assertEquals(Integer.toString(score2), config.getDriver().findElement(By.name("score2Team1")).getAttribute("value"));
    }

    @Test
    void testInsertScoreTeam2() {
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
    void testChangeCourtOrga(){
//        given
        Match match = createCompleteMatch();
        User user = createOrgaUser();
        loginUser(user);
        Court court = courtRepository.save(new Court(true, RandomStringUtils.randomAlphabetic(10)));
        court.setTournament(match.getRound().getTournament());
        courtRepository.save(court);
//        when
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
        Select courtSelect = new Select(config.getDriver().findElement(By.id("courtInput")));
        courtSelect.selectByVisibleText(court.getName());
        config.getDriver().findElement(By.id("courtFormSubmit")).click();
//        then
        match = matchRepository.findById(match.getId());
        Assertions.assertEquals(match.getCourt().getName(), court.getName());
        Assertions.assertEquals(match.getCourt().getId(), court.getId());
        Assertions.assertTrue(config.getDriver().findElement(By.id("courtFormSubmit")).isDisplayed());
    }

    @Test
    void testChangeCourtPlayer(){
//        given
        Match match = createCompleteMatch();
        User user = match.getTeams().get(0).getUserTournamentRoles().get(0).getUser();
        loginUser(user);
//        when
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
//        then
        WebDriver driver = config.getDriver();
        By elementId = By.id("courtFormSubmit");
        Assertions.assertThrows(NoSuchElementException.class, () -> {
            driver.findElement(elementId);
        });
    }

    @Test
    void testInsertScoreTeamOrga() {
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

    @Test
    void testNotificationEnterScore() throws InterruptedException {

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

        count.await(6,TimeUnit.SECONDS);

        config.getDriver().get(url + "/");
        config.getDriver().findElement(By.id("notLogged")).click();
        loginUser(match.getTeams().get(1).getUserTournamentRoles().get(0).getUser());

        Assertions.assertEquals("1",config.getDriver().findElement(new By.ById("unreadCount")).getText());

        config.getDriver().findElement(new By.ById("unreadCount")).click();
        WebElement notification = config.getDriver().findElement(new By.ById("notificationList")).findElements(new By.ByTagName("div")).get(0);
        notification.click();

        Assertions.assertEquals(url+"/matches/match?match_id=" + match.getId(),config.getDriver().getCurrentUrl());
    }

    @Test
    void testNotifificationNotDisplayMatchOver() throws InterruptedException {

        Match match = createCompleteMatch();
        loginUser(match.getTeams().get(0).getUserTournamentRoles().get(0).getUser());
        config.getDriver().get(url + "/matches/match?match_id=" + match.getId());
        int score1 = 50;
        int score2 = 25;
//        when
        config.getDriver().findElement(By.name("score1Team1")).clear();
        config.getDriver().findElement(By.name("score1Team1")).sendKeys(Integer.toString(score1));
        config.getDriver().findElement(By.name("score2Team1")).clear();
        config.getDriver().findElement(By.name("score2Team1")).sendKeys(Integer.toString(score2));
        config.getDriver().findElement(By.id("submitSet0")).click();

        count.await(6,TimeUnit.SECONDS);

        config.getDriver().get(url + "/");
        config.getDriver().findElement(By.id("notLogged")).click();
        loginUser(match.getTeams().get(1).getUserTournamentRoles().get(0).getUser());

        config.getDriver().findElement(new By.ById("unreadCount")).click();
        WebElement notification = config.getDriver().findElement(new By.ById("notificationList")).findElements(new By.ByTagName("div")).get(0);
        notification.click();

        config.getDriver().findElement(By.name("score1Team2")).clear();
        config.getDriver().findElement(By.name("score1Team2")).sendKeys(Integer.toString(score1));
        config.getDriver().findElement(By.name("score2Team2")).clear();
        config.getDriver().findElement(By.name("score2Team2")).sendKeys(Integer.toString(score2));
        config.getDriver().findElement(By.id("submitSet0")).click();

        count.await(6,TimeUnit.SECONDS);

        Assertions.assertEquals("1",config.getDriver().findElement(new By.ById("unreadCount")).getText());

        config.getDriver().get(url + "/");
        config.getDriver().findElement(By.id("notLogged")).click();
        loginUser(match.getTeams().get(0).getUserTournamentRoles().get(0).getUser());

        Assertions.assertEquals("1",config.getDriver().findElement(new By.ById("unreadCount")).getText());
    }

    void loginUser(User user) {
        config.getDriver().get(url + "/connexion");
        config.getDriver().findElement(new By.ById("email")).sendKeys(user.getEmail());
        config.getDriver().findElement(new By.ById("password")).sendKeys(user.getPassword());
        if (user.getUserTournamentRoles().get(0).getRole() != UserRole.STAFF) {
            config.getDriver().findElement(new By.ById("teamCode")).sendKeys(user.getUserTournamentRoles().get(0).getTeam().getCode());
        }
        config.getDriver().findElement(new By.ById("connexion")).click();
    }

    User createOrgaUser() {
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

        match.setCourt(courtRepository.save(new Court(true, RandomStringUtils.randomAlphabetic(10))));
        match.setSets(List.of(set1));
        match.setTeams(Arrays.asList(team1, team2));
        match.setNbSets(1);
        match = matchRepository.save(match);

        Tournament tournament = tournamentRepository.save(new Tournament());
        tournament.setName(RandomStringUtils.randomAlphabetic(10));
        tournament.setDate(Date.from(Instant.now()));

        Round round = new Round();
        Phase phase = new Phase();
        round.setTournament(tournament);
        round.setPhase(phase);
        round.setTeams(Arrays.asList(team1, team2));
        round.setType(PhaseType.SIMPLEGAME);
        phase.setTournament(tournament);
        phase.setRounds(List.of(round));

        phaseRepository.save(phase);
        tournament.setRounds(List.of(round));
        tournament.setPhases(List.of(phase));
        round.setMatches(List.of(match));
        match.setRound(round);
        tournamentRepository.save(tournament);
        roundRepository.save(round);
        matchRepository.save(match);

        return match;
    }

    @AfterAll
    void tearDown() {config.getDriver().quit();}
}

