package com.molkky.molkky;

import lombok.Getter;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;

@Getter
public class SeleniumConfig {

    private WebDriver driver;

    public SeleniumConfig() {
        driver = new ChromeDriver();
//        driver.manage().timeouts().implicitlyWait(5, TimeUnit.SECONDS);
    }

    static {
        if(System.getenv("CHROME_DRIVER") != null){
            System.setProperty("webdriver.chrome.driver", System.getenv("CHROME_DRIVER"));
        } else{
            System.setProperty("webdriver.chrome.driver", "bin/chromedriver99.exe");
        }
    }
}