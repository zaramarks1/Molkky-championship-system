package com.molkky.molkky.controllers;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/team")
public class TeamController {

    @GetMapping("/create")
    public String create(){
        return "";
    }
}
