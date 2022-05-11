package com.molkky.molkky.controllers;

import com.molkky.molkky.model.UserLogged;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.http.HttpSession;

@Controller
public class InfosController extends DefaultAttributes {

    @GetMapping("/infos")
    public String index(Model model, HttpSession session){
        return "infos";
    }
}
