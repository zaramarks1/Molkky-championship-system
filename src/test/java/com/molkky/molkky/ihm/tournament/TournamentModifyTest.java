package com.molkky.molkky.ihm.tournament;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.*;
import org.apache.commons.lang.RandomStringUtils;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import type.UserRole;

import java.text.ParseException;
import java.util.Arrays;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class TournamentModifyTest {
    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;

    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private PhaseRepository phaseRepository;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private TeamRepository teamRepository;

    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
    }

    @Test
    void testTournamentFormGetPage() {
        config.getDriver().get(url + "/tournament/allTournament");
        Assertions.assertEquals("Affichage des tournois", config.getDriver().getTitle());
    }

    @Test
    void testTournamentWrongAdm() throws ParseException {
        Tournament t = createTournament();
        loginUser(createFalseAdm());
        config.getDriver().get(url + "/tournament/view?tournamentId="+t.getId());

        config.getDriver().findElement(new By.ById("buttonModify")).click();

        Assertions.assertEquals("Affichage des tournois",config.getDriver().getTitle());
    }

    @Test
    void testTournamentModifyNbRound() throws ParseException {
        Tournament t = createTournament();
        List<UserTournamentRole> userTournamentRoles = userTournamentRoleRepository.findUserTournamentRoleByRoleAndTournament(UserRole.ADM,t);
        loginUser(userTournamentRoles.get(0).getUser());
        config.getDriver().get(url + "/tournament/view?tournamentId="+t.getId());


        config.getDriver().findElement(new By.ById("buttonModify")).click();

        Assertions.assertEquals("Modification du tournoi",config.getDriver().getTitle());

        config.getDriver().findElement(new By.ById("dateTournoi")).sendKeys("11/02/20");
        config.getDriver().findElement(new By.ById("cutOffDate")).sendKeys("11/02/20");
        config.getDriver().findElement(new By.ById("nbRounds")).clear();
        config.getDriver().findElement(new By.ById("nbRounds")).sendKeys("2");
        config.getDriver().findElement(new By.ById("sendTournament")).click();

        Assertions.assertEquals("Choix de la/des phase(s)",config.getDriver().getTitle());
    }

    @Test
    void testTournamentModifyRound() throws ParseException {
        Tournament t = createTournament();
        List<UserTournamentRole> userTournamentRoles = userTournamentRoleRepository.findUserTournamentRoleByRoleAndTournament(UserRole.ADM,t);
        loginUser(userTournamentRoles.get(0).getUser());

        config.getDriver().get(url + "/tournament/view?tournamentId="+t.getId());

        config.getDriver().findElement(new By.ById("buttonModify")).click();



        Assertions.assertEquals("Modification du tournoi",config.getDriver().getTitle());

        config.getDriver().findElement(new By.ById("nom")).clear();
        config.getDriver().findElement(new By.ById("nom")).sendKeys("Nom " + Math.floor(Math.random() * 1000));
        config.getDriver().findElement(new By.ById("dateTournoi")).sendKeys("11/02/20");
        config.getDriver().findElement(new By.ById("cutOffDate")).sendKeys("11/02/20");
        config.getDriver().findElement(new By.ById("sendTournament")).click();

        Assertions.assertEquals("Éditer les informations de la/des phase(s)",config.getDriver().getTitle());
        Assertions.assertEquals("Phase n°1 de type SIMPLEGAME",config.getDriver().findElement(new By.ByCssSelector("body > div > div.contentContainer > form > div:nth-child(1) > strong")).getText());
        Assertions.assertEquals("1",config.getDriver().findElement(new By.ById("sets")).getAttribute("value"));

        config.getDriver().findElement(By.id("sendPhases")).click();
        t = tournamentRepository.findById(t.getId());
        Assertions.assertEquals("Votre Tournoi",config.getDriver().getTitle());
        Assertions.assertEquals(t.getName(),config.getDriver().findElement(By.id("tournament_name")).getText());
    }

    @Test
    void testTournamentModifyNotification() throws ParseException {
        Tournament t = createTournament();
        List<UserTournamentRole> userTournamentRoles = userTournamentRoleRepository.findUserTournamentRoleByRoleAndTournament(UserRole.ADM,t);
        loginUser(userTournamentRoles.get(0).getUser());

        config.getDriver().get(url + "/tournament/view?tournamentId="+t.getId());
        config.getDriver().findElement(new By.ById("buttonModify")).click();

        Assertions.assertEquals("Modification du tournoi",config.getDriver().getTitle());

        config.getDriver().findElement(new By.ById("nom")).clear();
        config.getDriver().findElement(new By.ById("nom")).sendKeys("Nom " + Math.floor(Math.random() * 1000));
        config.getDriver().findElement(new By.ById("dateTournoi")).sendKeys("11/02/20");
        config.getDriver().findElement(new By.ById("cutOffDate")).sendKeys("11/02/20");
        config.getDriver().findElement(new By.ById("sendTournament")).click();

        config.getDriver().findElement(By.id("sendPhases")).click();
        t = tournamentRepository.findById(t.getId());
        loginUser(t.getTeams().get(0).getUserTournamentRoles().get(0).getUser());

        config.getDriver().findElement(new By.ById("unreadCount")).click();
        WebElement notification = config.getDriver().findElement(new By.ById("notificationList")).findElements(new By.ByTagName("div")).get(0);
        notification.click();

        Assertions.assertEquals("Votre Tournoi",config.getDriver().getTitle());
        Assertions.assertEquals(t.getName(),config.getDriver().findElement(By.id("tournament_name")).getText());
    }




    Tournament createTournament() throws ParseException {
        TournamentModel tournament = new TournamentModel();
        tournament.setName("Tournoi " + Math.floor(Math.random() * 1000));
        tournament.setLocation("location de test");
        tournament.setDate("2019-01-01");
        tournament.setCutOffDate("2019-01-01");
        tournament.setMinTeam(5);
        tournament.setMaxTeam(20);
        tournament.setNbRounds(1);
        tournament.setNbCourts(1);
        tournament.setNbPlayersPerTeam(1);
        tournament.setVisible(true);

        Tournament tournament1 = new Tournament(tournament);
        tournamentRepository.save(tournament1);

        SimpleGame simpleGame = new SimpleGame();
        simpleGame.setNbSets(1);
        simpleGame.setNbPhase(1);
        simpleGame.setNbTeamsQualified(1);
        simpleGame.setTournament(tournament1);
        phaseRepository.save(simpleGame);

        tournament1.setPhases(Arrays.asList(simpleGame));
        tournamentRepository.save(tournament1);

        UserTournamentRole userTournamentRole1 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user1 = new User();
        user1.setEmail(RandomStringUtils.randomAlphabetic(10) + "@gmail.com");
        user1.setPassword(RandomStringUtils.randomAlphabetic(10));
        user1.setUserTournamentRoles(List.of(userTournamentRole1));
        userRepository.save(user1);
        userTournamentRole1.setUser(user1);
        userTournamentRole1.setRole(UserRole.ADM);
        userTournamentRole1.setTournament(tournament1);
        userTournamentRoleRepository.save(userTournamentRole1);

        Team team = teamRepository.save(new Team());
        team.setCode(RandomStringUtils.randomAlphabetic(10));
        team.setTournament(tournament1);
        teamRepository.save(team);
        tournament1.setTeams(Arrays.asList(team));
        tournamentRepository.save(tournament1);

        UserTournamentRole userTournamentRole2 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user2 = new User();
        user2.setEmail(RandomStringUtils.randomAlphabetic(10) + "@gmail.com");
        user2.setPassword(RandomStringUtils.randomAlphabetic(10));
        user2.setUserTournamentRoles(List.of(userTournamentRole2));
        userRepository.save(user2);
        userTournamentRole2.setUser(user2);
        userTournamentRole2.setRole(UserRole.PLAYER);
        userTournamentRole2.setTournament(tournament1);
        userTournamentRole2.setTeam(team);
        userTournamentRoleRepository.save(userTournamentRole2);

        team.setUserTournamentRoles(Arrays.asList(userTournamentRole2));
        teamRepository.save(team);

        return tournament1;
    }

    void loginUser(User user) {
        config.getDriver().get(url + "/connexion");
        config.getDriver().findElement(new By.ById("email")).sendKeys(user.getEmail());
        config.getDriver().findElement(new By.ById("password")).sendKeys(user.getPassword());
        if (user.getUserTournamentRoles().get(0).getRole().equals(UserRole.PLAYER)) {
            config.getDriver().findElement(new By.ById("teamCode")).sendKeys(user.getUserTournamentRoles().get(0).getTeam().getCode());
        }
        config.getDriver().findElement(new By.ById("connexion")).click();
    }

    User createFalseAdm(){
        Tournament t = new Tournament();
        tournamentRepository.save(t);
        UserTournamentRole userTournamentRole2 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user2 = new User();
        user2.setEmail(RandomStringUtils.randomAlphabetic(10) + "@gmail.com");
        user2.setPassword(RandomStringUtils.randomAlphabetic(10));
        user2.setUserTournamentRoles(List.of(userTournamentRole2));
        userRepository.save(user2);
        userTournamentRole2.setUser(user2);
        userTournamentRole2.setRole(UserRole.ADM);
        userTournamentRole2.setTournament(t);

        userTournamentRoleRepository.save(userTournamentRole2);


        return user2;
    }
    /*
    @AfterAll
    void tearDown() {config.getDriver().quit();}

     */
}
