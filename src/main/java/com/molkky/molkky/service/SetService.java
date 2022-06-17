package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.model.UserModel;
import com.molkky.molkky.model.UserTournamentRoleModel;
import com.molkky.molkky.repository.CourtRepository;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.SetRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.SetTeamIndex;
import type.UserRole;

import java.util.*;

@Service
public class SetService {
    @Autowired
    private SetRepository setRepository;
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private MatchService matchService;
    @Autowired
    private CourtRepository courtRepository;
    @Autowired
    private NotificationService notificationService;

    @Autowired
    private UserTournamentRoleService userTournamentRoleService;

    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    private Timer timer = new Timer();

    static final String URLMATCH = "/matches/match?match_id=";

    public void enterSetResults(SetModel set, UserTournamentRoleModel user){
        Set setEntity = setScore(set,user);
        if(Boolean.TRUE.equals(isSetFinished(setEntity, user))){

            Set setSave = setRepository.findById(set.getId());
            if(setEntity.getScore1Orga()==0 && setEntity.getScore2Orga()==0 ){
                setEntity.setScore1Orga(setSave.getScore1Team1());
                setEntity.setScore2Orga(setSave.getScore2Team1());
            }
            setEntity.setFinished(true);
        }

        setEntity = setRepository.save(setEntity);

        Match match = setEntity.getMatch();

        if(Boolean.TRUE.equals(matchService.isMatchFinished(match))){
            match.setFinished(true);
            if(match.getCourt() != null) {
                match.getCourt().setAvailable(true);
                courtRepository.save(match.getCourt());
            }
            int scoreTeam1 =0;
            int scoreTeam2=0;

            for(Set s: match.getSets()){
                scoreTeam1 += s.getScore1Orga();
                scoreTeam2 += s.getScore2Orga();
            }

            if(scoreTeam1 >= scoreTeam2){
                match.setWinner(match.getTeams().get(0));
            }else{
                match.setWinner(match.getTeams().get(1));
            }
            match.setScoreTeam1(scoreTeam1);
            match.setScoreTeam2(scoreTeam2);

            match = matchRepository.save(match);

            matchService.validateMatch(match);
        }

        if(Boolean.FALSE.equals(isSetFinished(setEntity,user))){
            Team oppositeTeam = matchService.getOppositeTeam(MatchService.getMatchModelFromEntity(setEntity.getMatch()), user);
            List<UserTournamentRole> userRoles = userTournamentRoleRepository.findByTeamAndTournament(oppositeTeam,oppositeTeam.getTournament());
            for (UserTournamentRole userRole:userRoles){
                this.timerNotificationEnterScore(userRole,setEntity);
            }
        }
    }

    public Set setScore(SetModel set, UserTournamentRoleModel user){
        Set setEntity = getSetFromModel(set);
        SetTeamIndex teamIndex = matchService.getUserTeamIndex(MatchService.getMatchModelFromEntity(setEntity.getMatch()), user);
        switch (teamIndex){
            case TEAM1:
                setEntity.setScore1Team1(set.getScore1Team1());
                setEntity.setScore2Team1(set.getScore2Team1());
                break;
            case TEAM2:
                setEntity.setScore1Team2(set.getScore1Team2());
                setEntity.setScore2Team2(set.getScore2Team2());
                break;
            case ORGA:
                setEntity.setScore1Orga(set.getScore1Orga());
                setEntity.setScore2Orga(set.getScore2Orga());
                break;
            default:
                break;
        }
        return setEntity;
    }

    public Boolean isUserInSet(SetModel setModel, UserModel user){
        Set set = getSetFromModel(setModel);
        User userStaff = new User();
        UserTournamentRole userTournamentRole = new UserTournamentRole();
        userTournamentRole.setUser(userStaff);
        userTournamentRole.setRole(UserRole.STAFF);
        set.getMatch().setStaff(userStaff);
        return matchService.isUserInMatch(MatchService.getMatchModelFromEntity(set.getMatch()), user);
    }

    public static SetModel createSetModel(Set set){
        SetModel setModel = new SetModel();
        setModel.setId(set.getId());
        setModel.setScore1Team1(set.getScore1Team1());
        setModel.setScore2Team1(set.getScore2Team1());
        setModel.setScore1Team2(set.getScore1Team2());
        setModel.setScore2Team2(set.getScore2Team2());
        setModel.setScore1Orga(set.getScore1Orga());
        setModel.setScore2Orga(set.getScore2Orga());
        setModel.setScore1Final(set.getScore1Final());
        setModel.setScore2Final(set.getScore2Final());
        setModel.setFinished(set.getFinished());
        return setModel;
    }

    public static List<SetModel> createSetModels(List<Set> setList) {
        List<SetModel> setModelList = new ArrayList<>();
        for (Set set : setList) {
            setModelList.add(createSetModel(set));
        }
        return setModelList;
    }

    public Set getSetFromModel(SetModel setModel){
        return setRepository.findById(setModel.getId());
    }

    public Boolean isSetFinished(Set set, UserTournamentRoleModel user){
        if (set.getScore1Orga()==50 || set.getScore2Orga()==50){
            set.setScore1Final(set.getScore1Orga());
            set.setScore2Final(set.getScore2Orga());
            return true;
        }
        if((set.getScore1Team1() == 0 && set.getScore2Team1() == 0) || (set.getScore1Team2() == 0 && set.getScore2Team2() == 0)){
            return false;
        }
        if ((!Objects.equals(set.getScore1Team1(), set.getScore1Team2()))){
            notificationService.sendNotificationToList("Les scores rentrés par les deux équipes sont différents. Veuillez inscrire le score final.",URLMATCH+set.getMatch().getId(),userTournamentRoleService.getTournamentStaffFromUser(user));
            return false;
        }
        if ((!Objects.equals(set.getScore2Team1(), set.getScore2Team2()))){
            notificationService.sendNotificationToList("Les scores rentrés par les deux équipes sont différents. Veuillez inscrire le score final.",URLMATCH+set.getMatch().getId(),userTournamentRoleService.getTournamentStaffFromUser(user));
            return false;
        }

        if(set.getScore1Team1() == 50 || set.getScore2Team1() == 50) {
            set.setScore1Final(set.getScore1Team1());
            set.setScore2Final(set.getScore2Team1());
            return true;
        }

        return false;
    }

    public void timerNotificationEnterScore(UserTournamentRole user,Set set){
        timer.schedule(
                new TimerTask() {
                    @Override
                    public void run() {
                        notificationService.sendNotification("Veuillez entrer le score du match en cours",URLMATCH+set.getMatch().getId(),user);
                    }
                },
                5000
        );
    }
}
