package com.molkky.molkky;

import lombok.Getter;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.firefox.FirefoxOptions;

@Getter
public class SeleniumConfig {

    private WebDriver driver;

    public SeleniumConfig() {
        boolean isJenkins = System.getenv("JENKINS_HOME") != null;
        if(isJenkins) {
            FirefoxOptions options = new FirefoxOptions();
            options.setHeadless(true);
            options.addArguments("--no-sandbox");
            driver = new FirefoxDriver(options);
        }else {
            ChromeOptions options = new ChromeOptions();
            options.addArguments("--window-size=1920x1080");
            options.addArguments("--no-sandbox");
            driver = new ChromeDriver(options);
        }
    }

    static {
        if(System.getenv("CHROME_DRIVER") != null){
            System.setProperty("webdriver.chrome.driver", System.getenv("CHROME_DRIVER"));
        } else{
            System.setProperty("webdriver.chrome.driver", "bin/chromedriver99.exe");
        }
        if(System.getenv("FIREFOX_DRIVER") != null){
            System.setProperty("webdriver.gecko.driver", System.getenv("FIREFOX_DRIVER"));
        } else{
            System.setProperty("webdriver.gecko.driver", "bin/geckodriver");
        }
    }
}