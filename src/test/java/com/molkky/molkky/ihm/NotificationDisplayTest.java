package com.molkky.molkky.ihm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.NotificationService;
import org.apache.commons.lang.RandomStringUtils;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import type.UserRole;

import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class NotificationDisplayTest {
    private static final Logger logger = LoggerFactory.getLogger(NotificationDisplayTest.class);
    @Autowired
    private NotificationService notificationService;
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
    void testUnreadCountNoNotification() {
//        given
        User user = createUser();
        loginUser(user);
//        when
        config.getDriver().get(url + "/");
//        then
        Assertions.assertEquals(0, config.getDriver().findElements(new By.ById("unreadCount")).size());
    }

    @Test
    void testUnreadCountNotifications() {
//        given
        User user = createUser();
        loginUser(user);
//        when
        for(int i = 0; i < 5; i++){
            notificationService.sendNotification(Integer.toString(i), "http://localhost:8080/", user.getUserTournamentRoles().get(0));
        }
        config.getDriver().get(url + "/");
//        then
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("unreadCount")).isDisplayed());
        Assertions.assertEquals("5", config.getDriver().findElement(new By.ById("unreadCount")).getText());
    }

    @Test
    void testClickNotificationAndDisplay() {
//        given
        User user = createUser();
        loginUser(user);
//        when
        for(int i = 0; i < 5; i++){
            notificationService.sendNotification(Integer.toString(i), "https://www.google.fr/", user.getUserTournamentRoles().get(0));
        }
        config.getDriver().get(url + "/");
//        then
        WebElement notificationList = config.getDriver().findElement(new By.ById("notificationList"));
        Assertions.assertEquals("display: none;", notificationList.getAttribute("style")); ;

        WebDriverWait wait = new WebDriverWait(config.getDriver(), 30);
        config.getDriver().findElement(new By.ById("unreadCount")).click();
        wait.until(ExpectedConditions.visibilityOfElementLocated(new By.ById("notificationList")));


        notificationList = config.getDriver().findElement(new By.ById("notificationList"));
        Assertions.assertEquals("display: block;", notificationList.getAttribute("style")); ;
        WebElement notification = config.getDriver().findElement(new By.ById("notificationList")).findElements(new By.ByTagName("div")).get(0);
        notification.click();
        Assertions.assertEquals("https://www.google.fr/", config.getDriver().getCurrentUrl()); ;
    }

    void loginUser(User user){
        logger.info("Login user, username: " + user.getEmail() + ", password: " + user.getPassword());
        config.getDriver().get(url + "/connexion");
        config.getDriver().findElement(new By.ById("email")).sendKeys(user.getEmail());
        config.getDriver().findElement(new By.ById("password")).sendKeys(user.getPassword());
        if(user.getUserTournamentRoles().get(0).getRole() != UserRole.STAFF){
            config.getDriver().findElement(new By.ById("teamCode")).sendKeys(user.getUserTournamentRoles().get(0).getTeam().getCode());
        }
        config.getDriver().findElement(new By.ById("connexion")).click();
    }

    User createUser(){
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

    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}
