package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.http.HttpSession;
import java.util.List;

@Controller
public class InfosController {

    @Autowired
    MatchRepository matchRepository;
    @Autowired
    UserRepository userRepository;
    @GetMapping("/infos")
    public String index(Model model, HttpSession session){
        UserLogged user = (UserLogged)session.getAttribute("user");
        model.addAttribute("user", user);
        List<Match> matchList = matchRepository.findMatchAttributedToStaff(user.getTournament(),userRepository.findUserByEmail(user.getEmail()));
        model.addAttribute(matchList);
        return "infos";
    }
}
