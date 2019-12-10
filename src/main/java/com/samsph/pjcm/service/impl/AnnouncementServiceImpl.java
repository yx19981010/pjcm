package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.dao.AnnouncementRepository;
import com.samsph.pjcm.model.Announcement;
import com.samsph.pjcm.service.AnnouncementService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
@Transactional
public class AnnouncementServiceImpl implements AnnouncementService {
    @Autowired
    private AnnouncementRepository announcementRepository;

    public void addAnnouncement(Announcement announcement){
        announcementRepository.save(announcement);
    }

    public void updateAnnouncement(Announcement announcement){
        announcementRepository.save(announcement);
    }

    public void deleteAnnouncement(int id){
        announcementRepository.deleteById(id);
    }

    public Page<Announcement> findAnnouncements(PageRequest pageRequest){
        return announcementRepository.getAnnouncementList(pageRequest);
    }

    public Optional<Announcement> findAnnouncement(int id){
        return announcementRepository.findById(id);
    }
}
