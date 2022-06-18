package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.checkerframework.checker.units.qual.A;
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

    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private UserRepository userRepository;

    @GetMapping("/infos")
    public String index(Model model, HttpSession session){
        UserLogged userLogged = (UserLogged) session.getAttribute("user");
        User user = userRepository.findById(userLogged.getId());

        UserTournamentRole userTournamentRole = userTournamentRoleRepository.findUserTournamentRoleByUserId(user.getId());
        Tournament tournament = tournamentRepository.findById(userTournamentRole.getTournament().getId());

//        List<Match> matchList = matchRepository.findMatchAttributedToStaff(
//                tournament,
//                user
//        );
//        model.addAttribute(matchList);
        System.out.println(user.getId());
        System.out.println(userTournamentRole);
        return "infos";
    }
}
