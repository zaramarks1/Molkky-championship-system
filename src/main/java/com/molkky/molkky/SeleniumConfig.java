package com.molkky.molkky;

import lombok.Getter;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;

@Getter
public class SeleniumConfig {

    private WebDriver driver;

    public SeleniumConfig() {
        ChromeOptions options = new ChromeOptions();
        options.addArguments("--no-sandbox");

        boolean isJenkins = System.getenv("JENKINS_HOME") != null;
        if(isJenkins) {
            options.addArguments("--headless");
            options.addArguments("--disable-dev-shm-usage");
            options.addArguments("--window-size=1920x1080");
            options.setExperimentalOption("useAutomationExtension", false);
        }
        driver = new ChromeDriver(options);
    }

    static {
        if(System.getenv("CHROME_DRIVER") != null){
            System.setProperty("webdriver.chrome.driver", System.getenv("CHROME_DRIVER"));
        } else{
            System.setProperty("webdriver.chrome.driver", "bin/chromedriver100.exe");
        }
    }
}