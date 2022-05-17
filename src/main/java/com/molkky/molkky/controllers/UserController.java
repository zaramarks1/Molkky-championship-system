package com.molkky.molkky.controllers;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/user")
public class UserController {

    @GetMapping("/addPlayer")
    public String addPlayerForm(Model model){
        return "/team/addPlayer";
    }
}
