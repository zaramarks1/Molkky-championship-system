package com.molkky.molkky.ihm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.service.RegisterService;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.concurrent.TimeUnit;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class LoginTest {

    @Autowired
    private RegisterService registerService;
    @Autowired
    private TournamentRepository tournamentRepository;
    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;

    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
    }

    @Test
    void testLoginGetPage() {
        config.getDriver().get(url + "/connexion");
        Assertions.assertEquals("Page de connexion", config.getDriver().getTitle());
    }


    @Test
    void testConnexionDisplayed() {
        config.getDriver().get(url + "/connexion");
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("mail")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("code")).isDisplayed());
    }

    @Test
    void testConnexionUserGood() {
        config.getDriver().get(url + "/connexion");
        String user = "pierremenanteau@hotmail.fr";
        String code = "1,$.76?.15";
        config.getDriver().findElement(new By.ById("mail")).sendKeys(user);
        config.getDriver().findElement(new By.ById("code")).sendKeys(code);
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals(url + "/tournament/create", config.getDriver().getCurrentUrl());
    }

    @Test
    void testConnexionUserFalse() {
        config.getDriver().get(url + "/connexion");

        String user = "failTets@hotmail.fr";
        String code = "fail";
        config.getDriver().findElement(new By.ById("mail")).sendKeys(user);
        config.getDriver().findElement(new By.ById("code")).sendKeys(code);
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals(url + "/connexion", config.getDriver().getCurrentUrl());
    }

    @Test
    void testCreateUser() {

        String pwdRecept = "gpiTest1";
        String mailRecept = "gpiTestCalembel@gmail.com";

        // Go on google to connect to gmail for password
        config.getDriver().get("https://www.gmail.com");
        config.getDriver().findElement(By.xpath("//input[@id='identifierId']")).sendKeys(mailRecept);
        config.getDriver().findElement(By.xpath("//div[@id='identifierNext']")).click();
        config.getDriver().manage().timeouts().implicitlyWait(1, TimeUnit.SECONDS);
        config.getDriver().findElement(By.name( "password")).sendKeys(pwdRecept);
        config.getDriver().findElement(By.xpath("//div[@id='passwordNext']")).click();

        //Enter the sender mail id
        config.getDriver().manage().timeouts().implicitlyWait(5, TimeUnit.SECONDS);
        config.getDriver().findElement(By.xpath("//*[@id=':1m']")).click();
        String code = config.getDriver().findElement(By.xpath("//*[@id=':5o']")).getText();
        code = code.replaceAll("Tiens ton mdp : ","");
        Assertions.assertEquals(";+)35/?1.6", code);


/*
        config.getDriver().get(url + "/register");
        User user = new User();
        user.setEmail("userTest@gmail.com");
        config.getDriver().findElement(new By.ById("mail")).sendKeys(user.getEmail());
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals(url + "/connexion", config.getDriver().getCurrentUrl());
        // Récupérer le code envoyé par email
        /**user.setCode(ConnexionService.decodePwd(user));
        config.getDriver().findElement(new By.ById("mail")).sendKeys(user.getEmail());
        config.getDriver().findElement(new By.ById("code")).sendKeys(user.getCode());
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals(url + "/tournament/create", config.getDriver().getCurrentUrl());
    }

    @AfterAll
    void tearDown(){
        config.getDriver().quit();
    }
*/
    }
}
