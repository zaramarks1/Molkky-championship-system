package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.service.PhaseService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import type.UserRole;

import javax.servlet.http.HttpSession;
import java.util.List;
import java.util.Map;

@Controller
@RequestMapping("/phase")
public class PhaseController {

    @Autowired
    PhaseService phaseService;
    @GetMapping("/generate")
    public String generate(Model model, HttpSession session, @RequestParam(name = "phase_id", required = true) String id){

        UserLogged user = (UserLogged) session.getAttribute("user");

        if(user == null){
            return "redirect:/connexion";
        }
        if(user.getRole().equals(UserRole.ADM) ){
            Map<Round, List<Match>> response = phaseService.generate(id);

            model.addAttribute("round_match", response);
        }else{
            return "redirect:/";
        }


        return "redirect:/";

    }


}
