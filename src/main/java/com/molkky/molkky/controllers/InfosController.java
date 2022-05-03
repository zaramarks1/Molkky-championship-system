package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.UserLogged;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.http.HttpSession;

@Controller
public class InfosController {

    @GetMapping("/infos")
    public String Index(Model model, HttpSession session){
        UserLogged user = (UserLogged)session.getAttribute("user");
        model.addAttribute("user", user);
        return "infos";
    }
}
