package com.molkky.molkky.service;

import com.molkky.molkky.domain.Set;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.model.UserModel;
import com.molkky.molkky.repository.SetRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.SetTeamIndex;

import java.util.ArrayList;
import java.util.List;

@Service
public class SetService {
    @Autowired
    private SetRepository setRepository;

    @Autowired
    private MatchService matchService;

    public void enterSetResults(SetModel set, UserModel user){
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
        }
        setRepository.save(setEntity);
    }

    public Boolean isUserInSet(SetModel setModel, UserModel user){
        Set set = getSetFromModel(setModel);
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
}
