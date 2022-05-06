package com.molkky.molkky.controllers;

import com.molkky.molkky.service.TeamService;
import type.TournamentStatus;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.model.AddPlayerlistModel;
import com.molkky.molkky.model.CreateTeamModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.service.EmailSenderService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.List;

@Controller
@RequestMapping("/team")
public class TeamController {
    private static final Logger logger = LoggerFactory.getLogger(TeamController.class);

    @Autowired
    TeamRepository teamRepository;
    @Autowired
    TournamentRepository tournamentRepository;
    @Autowired
    UserRepository userRepository;
    @Autowired
    EmailSenderService emailSenderService;

    @Autowired
    TeamService teamService;



    @GetMapping("/create")
    public String create(Model model, HttpSession session){
        model.addAttribute("tournaments", tournamentRepository.findByVisibleAndStatus(true, TournamentStatus.AVAILABLE));
        model.addAttribute("team", new CreateTeamModel());
        User user = (User)session.getAttribute("user");
        model.addAttribute("user", user);
        return "/team/create";
    }


    @PostMapping("/create")
    public ModelAndView submit(@ModelAttribute("team") CreateTeamModel team, ModelMap model ){


        Team teamNew = teamService.create(team);

        model.addAttribute("team", teamNew);
        AddPlayerlistModel players = new AddPlayerlistModel();

        for(int i =0 ; i< teamNew.getTournament().getNbPlayersPerTeam();i++){
            players.addPlayer(new AddPlayerModel());

        }


        model.addAttribute("form", players);
        model.addAttribute("isDiffMail", true);
        return new ModelAndView( "/team/addPlayer", model) ;
    }


    @PostMapping("/addPlayer")
        public ModelAndView addPlayer(@ModelAttribute("form") AddPlayerlistModel form, ModelMap model){

        List<AddPlayerModel> players = form.getPlayers();
        List<User> users = new ArrayList<>();

        Team team = teamRepository.findById(players.get(0).getTeamId());

        for(AddPlayerModel player : players){
            User user = player.addPlayer();
            users.add(user);
        }

        if(!areAllDistinct(users)){
            model.addAttribute("team", team);
            model.addAttribute("isDiffMail", false);
            logger.trace("players must have diff email");
            return new ModelAndView( "/team/addPlayer", model) ;
        }

        teamService.addPlayers(form);
        return new ModelAndView( "redirect:/team/create", model) ;
        }

        boolean areAllDistinct(List<User> users) {
        return users.stream().map(User::getEmail).distinct().count() == users.size();
    }
}