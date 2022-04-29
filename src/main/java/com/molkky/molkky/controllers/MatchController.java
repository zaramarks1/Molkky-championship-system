package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.MatchModel;
import com.molkky.molkky.repository.MatchRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpSession;

@Controller
public class MatchController {
    @Autowired
    private MatchRepository matchRepository;

    @GetMapping("/matches/match")
    public String match(Model model, HttpSession session, @RequestParam(name = "match_id", required = true) Integer id) {
        User user = (User)session.getAttribute("user");
        model.addAttribute("user", user);

        MatchModel matchModel = new MatchModel(matchRepository.findById(id));
        model.addAttribute("match", matchModel);
        return "match/match";
    }
}
