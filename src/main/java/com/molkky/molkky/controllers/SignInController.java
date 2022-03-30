package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.service.SignInService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import java.util.List;

@Controller
public class SignInController {

    @Autowired
    private SignInService signInService;
    @GetMapping("/signin")
    public String Home(Model model){
        System.out.print("SHEEESH2");
        List<User> listUsers = signInService.getAllUsers();
        model.addAttribute("listUsers", listUsers);
        return "/signin";
    }

}
