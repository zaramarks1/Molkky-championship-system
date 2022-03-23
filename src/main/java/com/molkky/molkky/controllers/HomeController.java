package com.molkky.molkky.controllers;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
public class HomeController {

    @GetMapping("/home")
    public String Home(){
        System.out.print("SHEEESH");
        return "/home";
    }

}
