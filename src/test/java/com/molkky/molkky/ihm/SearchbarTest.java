package com.molkky.molkky.ihm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.utility.StringUtilities;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class SearchbarTest {
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
    void closeSearchResultsTest(){
//        given
        config.getDriver().get(url + "/");
//        when
        WebElement searchbar = config.getDriver().findElement(By.id("searchbarInput"));
        searchbar.sendKeys(StringUtilities.createCode(80));
        WebDriverWait wait = new WebDriverWait(config.getDriver(), 2);
        wait.until(ExpectedConditions.textToBe(By.id("searchbarResults"), "Pas de résultat pour la recherche"));

        Assertions.assertEquals("Pas de résultat pour la recherche", config.getDriver().findElement(By.id("searchbarResults")).getText());
//        then
        config.getDriver().findElement(By.className("bg")).click();
        Assertions.assertEquals("", config.getDriver().findElement(By.id("searchAutoresults")).getText());


    }
    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}
