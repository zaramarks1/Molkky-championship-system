package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.ClubRepository;
import com.molkky.molkky.service.TeamService;
import com.molkky.molkky.utility.FileUploadUtil;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
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
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

@Controller
@RequestMapping("/team")
public class TeamController extends DefaultAttributes {
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
    @Autowired
    ClubRepository clubRepository;

    @GetMapping("/create")
    public String create(Model model, HttpSession session){
        model.addAttribute("tournaments", tournamentRepository.findByVisibleAndStatus(true, TournamentStatus.AVAILABLE));
        model.addAttribute("team", new CreateTeamModel());
        model.addAttribute("clubs", clubRepository.findAll());
        UserLogged user = getUser(session);
        model.addAttribute("user", user);
        return "/team/create";
    }


    @PostMapping("/create")
    public ModelAndView submit(@ModelAttribute("team") CreateTeamModel team, ModelMap model, @RequestParam("photoFile") MultipartFile multipartFile) throws IOException {


        Team teamNew = teamService.create(team);
        //Start
        String fileNameString = StringUtils.cleanPath(multipartFile.getOriginalFilename());
        teamNew.setPhoto(fileNameString);
        Team teamSave = teamRepository.save(teamNew);
        String uploadDir = "./src/main/resources/static/teamPhotos/" + teamSave.getId();
        FileUploadUtil.saveFile(uploadDir, fileNameString, multipartFile);
        //Fin
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
            player.setClub(team.getClub());
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
        return new ModelAndView( "redirect:/tournament/view?tournamentId="+team.getTournament().getId(), model) ;
    }

    boolean areAllDistinct(List<User> users) {
        return users.stream().map(User::getEmail).distinct().count() == users.size();
    }

}