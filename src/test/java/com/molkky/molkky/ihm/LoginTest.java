package com.molkky.molkky.ihm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.User;
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
class LoginTest {

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

    //@Test
    void testConnexionDisplayed() {
        config.getDriver().get(url + "/connexion");
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("mail")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("code")).isDisplayed());
    }

    //@Test
    void testConnexionUserGood() {
        config.getDriver().get(url + "/connexion");
        String user = "pierremenanteau@hotmail.fr";
        String code = "1,$.76?.15";
        config.getDriver().findElement(new By.ById("mail")).sendKeys(user);
        config.getDriver().findElement(new By.ById("code")).sendKeys(code);
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals(url + "/tournament/create", config.getDriver().getCurrentUrl());
    }

   // @Test
    void testConnexionUserFalse() {
        config.getDriver().get(url + "/connexion");

        String user = "failTets@hotmail.fr";
        String code = "fail";
        config.getDriver().findElement(new By.ById("mail")).sendKeys(user);
        config.getDriver().findElement(new By.ById("code")).sendKeys(code);
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals(url + "/connexion", config.getDriver().getCurrentUrl());
    }


    //@Test
    void testConnectGmail(){
        //Connect to gmail
        String pwdRecept = "gpiTest1";
        String mailRecept = "gpiTestCalembel@gmail.com";
        config.getDriver().get("https://www.gmail.com");
        config.getDriver().findElement(By.xpath("//input[@id='identifierId']")).sendKeys(mailRecept);
        config.getDriver().findElement(By.xpath("//div[@id='identifierNext']")).click();
        config.getDriver().manage().timeouts().implicitlyWait(1, TimeUnit.SECONDS);
        config.getDriver().findElement(By.name("password")).sendKeys(pwdRecept);
        config.getDriver().findElement(By.xpath("//div[@id='passwordNext']")).click();

        //Open the mail and get the code
        config.getDriver().manage().timeouts().implicitlyWait(10, TimeUnit.SECONDS);
        config.getDriver().findElement(By.xpath("//*[@id=':1o']")).click();
        String code = config.getDriver().findElement(By.xpath("//*[@id=':5t']")).getText();
        System.out.println("Le code récuépéer par le driver est: " + code);
        code = code.replace("Tiens ton mdp: ", "");
        System.out.println("Le changement de code" + code);
        Assertions.assertEquals(";+)35/?1.6", code);
    }
    //@Test
    void testCreateUser() {
        config.getDriver().get(url + "/register");
        User user = new User();
        user.setEmail("gpiTestCalembel@gmail.com");
        config.getDriver().findElement(new By.ById("mail")).sendKeys(user.getEmail());
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals(url + "/connexion", config.getDriver().getCurrentUrl());
        config.getDriver().manage().timeouts().implicitlyWait(10, TimeUnit.SECONDS);

        // Go on google to connect to gmail for password
        String pwdRecept = "gpiTest1";
        String mailRecept = "gpiTestCalembel@gmail.com";
        config.getDriver().get("https://www.gmail.com");
        config.getDriver().findElement(By.xpath("//input[@id='identifierId']")).sendKeys(mailRecept);
        config.getDriver().findElement(By.xpath("//div[@id='identifierNext']")).click();
        config.getDriver().manage().timeouts().implicitlyWait(1, TimeUnit.SECONDS);
        config.getDriver().findElement(By.name("password")).sendKeys(pwdRecept);
        config.getDriver().findElement(By.xpath("//div[@id='passwordNext']")).click();

        //Enter the sender mail id
        config.getDriver().manage().timeouts().implicitlyWait(10, TimeUnit.SECONDS);
        config.getDriver().findElement(By.xpath("//*[@id=':1s']")).click();
        String code = config.getDriver().findElement(By.xpath("//*[@id=':5t']")).getText();
        System.out.println("Le code récuépéer par le driver est: " + code);
        code = code.replace("Tiens ton mdp: ", "");
        System.out.println("Le changement de code" + code);
        Assertions.assertEquals(";+)35/?1.6", code);

        //Connect to the molkky connection page
        config.getDriver().get(url + "/connexion");
        config.getDriver().findElement(new By.ById("mail")).sendKeys(user.getEmail());
        config.getDriver().findElement(new By.ById("code")).sendKeys(code);
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals(url + "/tournament/create", config.getDriver().getCurrentUrl());
    }

    @AfterAll
    void tearDown(){
        config.getDriver().quit();
    }
}
