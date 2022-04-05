package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.model.AddPlayerlistModel;
import com.molkky.molkky.model.CreateTeamModel;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.ArrayList;
import java.util.List;

@Controller
@RequestMapping("/team")
public class TeamController {

    @Autowired
    TeamRepository teamRepository;
    @Autowired
    TournamentRepository tournamentRepository;
    @Autowired
    UserRepository userRepository;



    @GetMapping("/create")
    public String create(Model model){
        model.addAttribute("tournaments", tournamentRepository.findAll());
        model.addAttribute("team", new CreateTeamModel());
        return "/team/create";
    }


    @PostMapping("/create")
    public ModelAndView submit(@ModelAttribute("team") CreateTeamModel team, ModelMap model ){


        Integer idTournament = Integer.valueOf(team.getTournament());
        Tournament tournament = tournamentRepository.findById(idTournament);

        //System.out.println(team.getTournament().getName());
        Team teamCreate = new Team();

        teamCreate.setName(team.getName());
        teamCreate.setNbPlayers(team.getNbPlayers());
        teamCreate.setTournament(tournament);

        Team teamNew = teamRepository.save(teamCreate);

        model.addAttribute("team", teamNew);
        AddPlayerlistModel players = new AddPlayerlistModel();
        for(int i =0 ; i< teamNew.getNbPlayers();i++){
            players.addPlayer(new AddPlayerModel());
        }
        model.addAttribute("form", players);
        return new ModelAndView( "/team/addPlayer", model) ;
    }


    @PostMapping("/addPlayer")
        public String addPlayer(@ModelAttribute("form") AddPlayerlistModel form, Model model){

        List<AddPlayerModel> players = form.getPlayers();
        List<User> users = new ArrayList<>();

        for(AddPlayerModel player : players){
            Team team = teamRepository.findById(player.getTeamId());
            User user = player.addPlayer();
            user.setTeam(team);
            users.add(user);
        }
        userRepository.saveAll(users);
            return "redirect:/team/create";
        }
}