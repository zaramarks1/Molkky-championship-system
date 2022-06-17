package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.http.HttpSession;
import java.util.List;

@Controller
public class InfosController extends DefaultAttributes {

    @Autowired
    private MatchRepository matchRepository;

    @GetMapping("/infos")
    public String index(Model model, HttpSession session){
        //List<Match> matches = matchRepository.findMatchAttributedToStaff();
        return "infos";
    }
}
