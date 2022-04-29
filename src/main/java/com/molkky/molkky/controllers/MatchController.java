package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.*;
import com.molkky.molkky.repository.MatchRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpSession;
import java.util.Arrays;

@Controller
public class MatchController {
    @Autowired
    private MatchRepository matchRepository;

    @GetMapping("/matches/match")
    public String match(Model model, HttpSession session, @RequestParam(name = "match_id", required = true) Integer id) {
        User user = (User)session.getAttribute("user");
        model.addAttribute("user", user);
        Match match = matchRepository.findById(id);
        model.addAttribute("match", new MatchModel(match));
        model.addAttribute("teams", Arrays.asList(new TeamModel(match.getTeams().get(0)), new TeamModel(match.getTeams().get(1))));
        model.addAttribute("court", new CourtModel(match.getCourt()));
        model.addAttribute("tournament", new TournamentModel(match.getRound().getTournament()));
        model.addAttribute("sets", SetModel.createSetModels(match.getSets()));
        return "match/match";
    }
}
